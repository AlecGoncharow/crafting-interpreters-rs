use crate::ast::{BinaryExpr, Expr, LogicalExpr, Statement, StatementBlock, UnaryExpr};
use crate::environment::Environment;
use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenLiteral;
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
    //pub closure: Environment,
}

impl Function {
    pub fn new(params: &[Token], body: &[Statement]) -> Self {
        Self {
            params: params.into(),
            body: Vec::from(body).into(),
        }
    }

    pub fn new_callable(params: &[Token], body: &[Statement]) -> Callable {
        Callable::Function(Self::new(params, body))
    }
}

impl Callable {
    pub fn call(&mut self, environment: &mut Environment, args: Vec<Value>) -> RuntimeResult {
        match self {
            Self::Clock => {
                use std::time::SystemTime;

                let now: Value = (SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .expect("time machine broke")
                    .as_secs() as f64)
                    .into();

                println!("{:?}", now);
                Ok(now)
            }
            Self::Function(function) => {
                let mut environment = Environment::new_enclosed(environment.clone());

                for i in 0..function.params.len() {
                    environment.define(
                        &function.params.get(i).unwrap().lexeme,
                        args.get(i).unwrap().clone().into(),
                    );
                }

                function.body.execute(&mut environment)
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

#[derive(Debug)]
pub enum ExecutorError {
    RuntimeError(Token, String),
}

pub type RuntimeResult = Result<Value, ExecutorError>;

pub trait Executable {
    fn execute(&self, environment: &mut Environment) -> RuntimeResult;
}

pub trait Interpretable {
    fn interpret(&self, environment: &mut Environment) -> RuntimeResult;
}

impl Interpretable for Expr {
    fn interpret(&self, environment: &mut Environment) -> RuntimeResult {
        match self {
            Expr::Assign(token, expr) => {
                let val = expr.interpret(environment)?;
                environment.assign(&token.lexeme, val.into())?;
                Ok(Value::Nil)
            }
            Expr::Binary(inner) => inner.interpret(environment),
            Expr::Unary(inner) => inner.interpret(environment),

            Expr::Call(callee, paren, arguments) => {
                let mut function = match callee.interpret(environment)? {
                    Value::Callable(inner) => inner,
                    _ => {
                        println!("{:?}", callee);
                        return Err(ExecutorError::RuntimeError(
                            paren.clone(),
                            "Can only call functions and classes.".into(),
                        ));
                    }
                };

                if arguments.len() != function.arity() {
                    return Err(ExecutorError::RuntimeError(
                        paren.clone(),
                        format!(
                            "Expected {} args but got {}.",
                            function.arity(),
                            arguments.len()
                        ),
                    ));
                }

                let mut args = Vec::new();
                for arg in arguments {
                    args.push(arg.interpret(environment)?);
                }

                function.call(environment, args)
            }

            Expr::Logical(inner) => inner.interpret(environment),

            Expr::Grouping(expression) => expression.interpret(environment),
            Expr::Literal(literal) => Ok(literal.clone().into()),
            Expr::Variable(token) => {
                let lookup = environment.get(&token.lexeme)?;

                match lookup {
                    Value::Uninit => Err(ExecutorError::RuntimeError(
                        token.clone(),
                        "Variable used before initalization".into(),
                    )),
                    _ => Ok(lookup.clone()),
                }
            }
        }
    }
}

impl Interpretable for BinaryExpr {
    fn interpret(&self, environment: &mut Environment) -> RuntimeResult {
        let left = self.left.interpret(environment)?;
        let right = self.right.interpret(environment)?;

        Ok(match self.operator.kind {
            TokenKind::MINUS => {
                check_number_operands(&self.operator, &left, &right)?;
                (left.number().unwrap() - right.number().unwrap()).into()
            }

            TokenKind::SLASH => {
                check_number_operands(&self.operator, &left, &right)?;
                (left.number().unwrap() / right.number().unwrap()).into()
            }

            TokenKind::STAR => {
                check_number_operands(&self.operator, &left, &right)?;
                (left.number().unwrap() * right.number().unwrap()).into()
            }

            TokenKind::PLUS => {
                if let Value::Str(mut left_s) = left {
                    if let Value::Str(right_s) = right {
                        left_s.push_str(&right_s);
                        return Ok(left_s.into());
                    }
                } else if let Value::Number(left_n) = left {
                    if let Value::Number(right_n) = right {
                        return Ok((left_n + right_n).into());
                    }
                }
                return Err(ExecutorError::RuntimeError(
                    self.operator.clone(),
                    "Operands must be two numbers or two strings.".into(),
                ));
            }

            TokenKind::GREATER => {
                check_number_operands(&self.operator, &left, &right)?;
                (left.number().unwrap() > right.number().unwrap()).into()
            }

            TokenKind::GREATER_EQUAL => {
                check_number_operands(&self.operator, &left, &right)?;
                (left.number().unwrap() >= right.number().unwrap()).into()
            }

            TokenKind::LESS => {
                check_number_operands(&self.operator, &left, &right)?;
                (left.number().unwrap() < right.number().unwrap()).into()
            }

            TokenKind::LESS_EQUAL => {
                check_number_operands(&self.operator, &left, &right)?;
                (left.number().unwrap() <= right.number().unwrap()).into()
            }

            TokenKind::EQUAL_EQUAL => (left == right).into(),

            _ => unreachable!(),
        })
    }
}

impl Interpretable for UnaryExpr {
    fn interpret(&self, environment: &mut Environment) -> RuntimeResult {
        let right = self.expr.interpret(environment)?;
        Ok(match self.operator.kind {
            TokenKind::MINUS => {
                check_number_operand(&self.operator, &right)?;
                let val = -right.number().unwrap();
                val.into()
            }
            TokenKind::BANG => (!right.is_truthy()).into(),
            _ => unreachable!(),
        })
    }
}

impl Interpretable for LogicalExpr {
    fn interpret(&self, environment: &mut Environment) -> RuntimeResult {
        // only evaluate left to possibly short circut
        let left = self.left.interpret(environment)?;
        match self.operator.kind {
            TokenKind::OR => {
                if left.is_truthy() {
                    return Ok(left.into());
                }
            }

            TokenKind::AND => {
                if !left.is_truthy() {
                    return Ok(left.into());
                }
            }

            _ => unreachable!(),
        }

        // couldn't short circut, eval right
        self.right.interpret(environment)
    }
}

impl Executable for Statement {
    fn execute(&self, environment: &mut Environment) -> RuntimeResult {
        match self {
            Statement::Expr(expr) | Statement::ForIncr(expr) => expr.interpret(environment),
            Statement::If(cond, then_branch, else_branch) => {
                let out = cond.interpret(environment)?;

                if out.is_truthy() {
                    then_branch.execute(environment)
                } else {
                    else_branch.execute(environment)
                }
            }
            Statement::Function(name, args, body, _env) => {
                /*
                let env = if env.is_some() {
                    println!("{:#?}", env);
                    env.clone()
                } else {
                    Some(Environment::new_enclosed(environment.clone()))
                };
                */

                environment.define(&name.lexeme, Function::new_callable(args, body).into());
                Ok(Value::Nil)
            }
            Statement::Print(expr) => {
                let value = expr.interpret(environment)?;
                println!("{}", value);
                Ok(Value::Nil)
            }
            Statement::Var(token, expr) => {
                let val = expr.interpret(environment)?;
                environment.define(&token.lexeme, val.into());
                Ok(Value::Nil)
            }
            Statement::While(expr, stmt) => loop {
                let out = expr.interpret(environment)?;

                if !out.is_truthy() {
                    return Ok(Value::Nil);
                }
                match stmt.execute(environment)? {
                    Value::Break => return Ok(Value::Nil),
                    Value::Return(value) => return Ok(*value),
                    _ => (),
                }
            },
            Statement::Block(block) => block.execute(environment),
            Statement::Return(_keyword, value) => {
                Ok(Value::Return(value.interpret(environment)?.into()))
            }
        }
    }
}

impl Executable for StatementBlock {
    fn execute(&self, environment: &mut Environment) -> RuntimeResult {
        // make inner env our new env
        *environment = Environment::new_enclosed(environment.clone());

        for stmt in &self.statements {
            match stmt.execute(environment)? {
                Value::Break => break,
                Value::Return(value) => {
                    return Ok(*value);
                }
                Value::Continue => {
                    // need to do increment in for loop if continue'd, check if last stmt is ForIncr
                    if let Statement::ForIncr(expr) = self.statements.last().unwrap() {
                        expr.interpret(environment)?;
                    }
                    break;
                }
                _ => (),
            }
        }

        // return out env to main env
        *environment = *environment.clone().into_enclosing().unwrap();

        Ok(Value::Nil)
    }
}

fn check_number_operand(operator: &Token, operand: &Value) -> Result<(), ExecutorError> {
    if operand.number().is_some() {
        Ok(())
    } else {
        Err(ExecutorError::RuntimeError(
            operator.clone(),
            "Operand must be a number.".into(),
        ))
    }
}

fn check_number_operands(
    operator: &Token,
    left: &Value,
    right: &Value,
) -> Result<(), ExecutorError> {
    if left.number().is_some() && right.number().is_some() {
        Ok(())
    } else {
        Err(ExecutorError::RuntimeError(
            operator.clone(),
            "Operand must be a numbers.".into(),
        ))
    }
}

pub struct Interpreter {
    // this might be awful
    pub stack: Vec<TokenLiteral>,
    pub environment: Rc<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut environment = Environment::new();
        environment.define("clock", Value::Callable(Callable::Clock));

        Self {
            stack: Vec::new(),
            environment: environment.into(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Statement]) -> RuntimeResult {
        for stmt in stmts {
            stmt.execute(&mut Rc::get_mut(&mut self.environment).unwrap())?;
        }
        Ok(Value::Nil)
    }
}

impl From<Callable> for Value {
    fn from(c: Callable) -> Self {
        Self::Callable(c)
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
