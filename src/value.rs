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
    Clock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub params: Vec<Token>,
    pub body: StatementBlock,
    pub closure: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(params: &[Token], body: StatementBlock, closure: Rc<RefCell<Environment>>) -> Self {
        Self {
            params: params.into(),
            body,
            closure,
        }
    }

    pub fn new_callable(
        params: &[Token],
        body: StatementBlock,
        closure: Rc<RefCell<Environment>>,
    ) -> Callable {
        Callable::Function(Self::new(params, body, closure))
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
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Clock => 0,
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
            Self::Callable(inner) => write!(f, "callable {:?}", inner),
            Self::Return(..) => write!(f, "return"),
        }
    }
}
