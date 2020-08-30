use crate::ast::StatementBlock;
use crate::environment::Environment;
use crate::interpreter::Executable;
use crate::interpreter::Interpreter;
use crate::interpreter::RuntimeResult;
use crate::token::Token;
use std::cell::RefCell;
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
                let instance = Self::ClassInstance(ClassInstance {
                    class: class.clone(),
                });

                Ok(Value::Callable(instance))
            }
            Self::ClassInstance(intance) => {
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
