use crate::ast::StatementBlock;
use crate::environment::Environment;
use crate::interpreter::Executable;
use crate::interpreter::ExecutorError;
use crate::interpreter::Interpreter;
use crate::interpreter::RuntimeResult;
use crate::token::Token;
use crate::token::TokenLiteral;
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
}

impl Class {
    pub fn new(name: String) -> Self {
        Self { name }
    }

    pub fn new_callable(name: String) -> Callable {
        Callable::Class(Self::new(name))
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

    pub fn new_callable(class: &Class) -> Callable {
        Callable::ClassInstance(Self::new(class))
    }

    pub fn get(&self, name: &Token) -> RuntimeResult {
        if let Some(value) = self.fields.get(&name.lexeme) {
            Ok(value.clone())
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
}

impl Function {
    pub fn new(
        name: String,
        params: &[Token],
        body: StatementBlock,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            name,
            params: params.into(),
            body,
            closure,
        }
    }

    pub fn new_callable(
        name: String,
        params: &[Token],
        body: StatementBlock,
        closure: Rc<RefCell<Environment>>,
    ) -> Callable {
        Callable::Function(Self::new(name, params, body, closure))
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

                function.body.execute(interpreter, environment)
            }
            Self::Class(class) => {
                let instance = ClassInstance::new_callable(class);

                Ok(Value::Callable(instance))
            }
            Self::ClassInstance(_instance) => {
                unimplemented!();
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Clock | Self::Class(_) | Self::ClassInstance(_) => 0,
            Self::Function(function) => function.params.len(),
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
