use crate::ast::{BinaryExpr, Expr, LogicalExpr, Statement, StatementBlock, UnaryExpr};
use crate::environment::Environment;
use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenLiteral;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Str(String),
    Identifier(String),
    Break,
    Continue,
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
            TokenLiteral::Identifier(s) => Self::Identifier(s),
            TokenLiteral::Str(s) => Self::Str(s),
            TokenLiteral::Number(n) => Self::Number(n),
            TokenLiteral::Bool(b) => Self::Bool(b),
            TokenLiteral::None => Self::Nil,
            _ => unimplemented!(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Identifier(s) => write!(f, "{}", s),
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
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Return(..) => write!(f, "return"),
        }
    }
}

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

pub struct Interpreter {
    // this might be awful
    pub stack: Vec<TokenLiteral>,
    pub environment: Environment,
}

impl Interpretable for Expr {
    fn interpret(&self, environment: &mut Environment) -> RuntimeResult {
        match self {
            Expr::Assign(token, expr) => {
                let val = expr.interpret(environment)?;
                println!("assign | name: {:?} val: {:?}", token, val);
                unimplemented!();
                //environment.assign(&token.lexeme, val.into())?;
            }
            Expr::Binary(inner) => return inner.interpret(environment),
            Expr::Unary(inner) => return inner.interpret(environment),

            Expr::Call(callee, paren, arguments) => {
                let callee_name = callee.interpret(environment)?;
                let mut function = environment.get(&callee_name.to_string())?.clone();
                function = match function {
                    Statement::Function(name, params, body, closure) => {
                        Statement::Function(name, params, body, closure)
                    }
                    _ => {
                        println!("{:?}", function);
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

                //let function_val = function.call(self, args)?;
                //function_val.execute(environment)?;
            }

            Expr::Logical(inner) => return inner.interpret(environment),

            Expr::Grouping(expression) => return expression.interpret(environment),
            Expr::Literal(literal) => return Ok(literal.clone().into()),
            Expr::Variable(token) => {
                let lookup = environment.get(&token.lexeme);

                unimplemented!();
            }
        }

        Ok(Value::Nil)
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
            Statement::Function(name, args, body, env) => {
                let env = if env.is_some() {
                    println!("{:#?}", env);
                    env.clone()
                } else {
                    Some(Environment::new_enclosed(environment.clone()))
                };

                environment.define(
                    &name.lexeme,
                    Statement::Function(name.clone(), args.clone(), body.clone(), env),
                );
                Ok(Value::Nil)
            }
            Statement::Print(expr) => {
                let value = expr.interpret(environment)?;
                println!("{}", value);
                Ok(Value::Nil)
            }
            Statement::Var(token, expr) => {
                let val = expr.interpret(environment)?;
                unimplemented!();
                //environment.define(&token.lexeme, val.into());
                Ok(Value::Nil)
            }
            Statement::While(expr, stmt) => loop {
                let out = expr.interpret(environment)?;

                if !out.is_truthy() {
                    return Ok(Value::Nil);
                }
                match stmt.execute(environment)? {
                    Value::Break => return Ok(Value::Nil),
                    Value::Return(_value) => unimplemented!(),
                    _ => (),
                }
            },
            Statement::Block(block) => unimplemented!(), //block.execute(environment),
            Statement::Return(_keyword, value) => value.interpret(environment),
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
                Value::Return(_value) => {
                    break;
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

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Statement]) -> RuntimeResult {
        for stmt in stmts {
            stmt.execute(&mut self.environment)?;
        }
        Ok(Value::Nil)
    }
}
