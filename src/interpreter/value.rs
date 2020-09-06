use crate::interpreter::ast::StatementBlock;
use crate::interpreter::environment::Environment;
use crate::interpreter::Executable;
use crate::interpreter::ExecutorError;
use crate::interpreter::Interpreter;
use crate::interpreter::RuntimeResult;
use super::token::Token;
use super::token::TokenLiteral;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    Function(Function),
    Class(Class),
    ClassInstance(ClassInstance),
    Clock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Function>,
}

impl Class {
    pub fn new(name: String, methods: HashMap<String, Function>) -> Self {
        Self { name, methods }
    }

    pub fn new_callable(name: String, methods: HashMap<String, Function>) -> Callable {
        Callable::Class(Self::new(name, methods))
    }

    pub fn find_method(&self, name: &str) -> Option<&Function> {
        if let Some(method) = self.methods.get(name) {
            Some(method)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassInstance {
    pub class: Class,
    pub fields: HashMap<String, Value>,
}

impl ClassInstance {
    pub fn new(class: &Class) -> Self {
        Self {
            class: class.clone(),
            fields: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    pub fn new_callable(class: &Class) -> Callable {
        Callable::ClassInstance(Self::new(class))
    }

    pub fn get(&self, name: &Token) -> RuntimeResult {
        if let Some(value) = self.fields.get(&name.lexeme) {
            Ok(value.clone())
        } else if let Some(method) = self.class.find_method(&name.lexeme) {
            Ok(Value::Callable(Callable::Function(
                method.clone().bind(self),
            )))
        } else {
            Err(ExecutorError::RuntimeError(
                name.clone(),
                format!("Undefined property '{}'.", name.lexeme),
            ))
        }
    }

    pub fn set(&mut self, name: &Token, value: Value) {
        self.fields.insert(name.lexeme.clone(), value);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Token>,
    pub body: StatementBlock,
    pub closure: Rc<RefCell<Environment>>,
    pub is_init: bool,
}

impl Function {
    pub fn new(
        name: String,
        params: &[Token],
        body: StatementBlock,
        closure: Rc<RefCell<Environment>>,
        is_init: bool,
    ) -> Self {
        Self {
            name,
            params: params.into(),
            body,
            closure,
            is_init,
        }
    }

    pub fn new_callable(
        name: String,
        params: &[Token],
        body: StatementBlock,
        closure: Rc<RefCell<Environment>>,
        is_init: bool,
    ) -> Callable {
        Callable::Function(Self::new(name, params, body, closure, is_init))
    }

    pub fn bind(self, instance: &ClassInstance) -> Self {
        let mut environment = Environment::new_enclosed(self.closure.clone());
        environment.define("this", instance.clone().into());
        Self::new(
            self.name,
            &self.params,
            self.body,
            Rc::new(RefCell::new(environment)),
            self.is_init,
        )
    }
}

impl Callable {
    pub fn call(
        &mut self,
        interpreter: &mut Interpreter,
        _environment: Rc<RefCell<Environment>>,
        args: Vec<Value>,
    ) -> RuntimeResult {
        match self {
            Self::Clock => {
                use std::time::SystemTime;

                let now: Value = (SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .expect("time machine broke")
                    .as_secs() as f64)
                    .into();

                Ok(now)
            }
            Self::Function(function) => {
                let environment = Rc::new(RefCell::new(Environment::new_enclosed(
                    function.closure.clone(),
                )));

                for i in 0..function.params.len() {
                    environment.borrow_mut().define(
                        &function.params.get(i).unwrap().lexeme,
                        args.get(i).unwrap().clone().into(),
                    );
                }

                let out = function.body.execute(interpreter, environment)?;

                if function.is_init {
                    let mut this = Token::none();
                    this.lexeme = "this".into();
                    Ok(function.closure.borrow().get_at(0, &this)?.clone())
                } else {
                    Ok(out)
                }
            }
            Self::Class(class) => {
                let mut instance = ClassInstance::new(class);
                if let Some(initializer) = class.find_method("init") {
                    let func = initializer.clone().bind(&instance);
                    let out = Self::Function(func.clone()).call(interpreter, _environment, args)?;

                    if let Value::Callable(Callable::ClassInstance(inner)) = out {
                        println!("{:?}", inner.fields);
                        instance = inner;
                    }
                }

                Ok(Value::Callable(instance.into()))
            }
            Self::ClassInstance(_instance) => {
                unimplemented!();
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Clock | Self::ClassInstance(_) => 0,
            Self::Function(function) => function.params.len(),
            Self::Class(class) => {
                if let Some(initializer) = class.find_method("init") {
                    initializer.params.len()
                } else {
                    0
                }
            }
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Uninit,
    Nil,
    Bool(bool),
    Number(f64),
    Str(String),
    Break,
    Continue,
    Callable(Callable),
    Return(Box<Value>),
}

impl Value {
    pub fn number(&self) -> Option<f64> {
        if let Self::Number(inner) = self {
            Some(*inner)
        } else {
            None
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::Nil => false,
            _ => true,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Number(n) => {
                let mut s = n.to_string();
                if s.ends_with(".0") {
                    s.truncate(s.len() - 2);
                }

                write!(f, "{}", s)
            }
            Self::Bool(b) => write!(f, "{}", b.to_string()),
            Self::Nil => write!(f, "nil"),
            Self::Uninit => write!(f, "uninit"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Callable(inner) => write!(f, "callable {}", inner),
            Self::Return(..) => write!(f, "return"),
        }
    }
}

impl fmt::Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Clock => write!(f, "std::Clock"),
            Self::Function(fun) => write!(f, "Function {:?}", fun.name),
            Self::Class(class) => write!(f, "Class {:?}", class.name),
            Self::ClassInstance(inst) => write!(f, "{:?} instance", inst.class.name),
        }
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self::Str(v.into())
    }
}

impl From<String> for Value {
    fn from(v: String) -> Self {
        Self::Str(v.into())
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Number(v.into())
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Bool(v.into())
    }
}

impl From<TokenLiteral> for Value {
    fn from(v: TokenLiteral) -> Self {
        match v {
            TokenLiteral::Identifier(s) => Self::Str(s),
            TokenLiteral::Str(s) => Self::Str(s),
            TokenLiteral::Number(n) => Self::Number(n),
            TokenLiteral::Bool(b) => Self::Bool(b),
            TokenLiteral::None => Self::Nil,
            TokenLiteral::Uninit => Self::Uninit,
            TokenLiteral::Continue => Self::Continue,
            TokenLiteral::Break => Self::Break,
        }
    }
}

impl From<Function> for Callable {
    fn from(func: Function) -> Self {
        Self::Function(func)
    }
}

impl From<Callable> for Value {
    fn from(callable: Callable) -> Self {
        Self::Callable(callable)
    }
}

impl From<ClassInstance> for Value {
    fn from(inst: ClassInstance) -> Self {
        Self::Callable(inst.into())
    }
}

impl From<ClassInstance> for Callable {
    fn from(inst: ClassInstance) -> Self {
        Callable::ClassInstance(inst)
    }
}
