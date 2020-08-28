use crate::ast::{BinaryExpr, Expr, LogicalExpr, Statement, StatementBlock, UnaryExpr};
use crate::environment::Environment;
use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenLiteral;
use std::cell::RefCell;
use std::collections::HashMap;
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

                println!("{:?}", now);
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

#[derive(Debug)]
pub enum ExecutorError {
    RuntimeError(Token, String),
}

pub type RuntimeResult = Result<Value, ExecutorError>;

pub trait Executable {
    fn execute(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult;
}

pub trait Interpretable {
    fn interpret(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult;
}

impl Interpretable for Expr {
    fn interpret(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult {
        match self {
            Expr::Assign(token, expr) => {
                let val = expr.interpret(interpreter, environment.clone())?;

                if let Some(distance) = interpreter.locals.get(&token.lexeme) {
                    environment
                        .borrow_mut()
                        .assign_at(*distance, &token.lexeme, val.into())?;
                } else {
                    environment.borrow_mut().assign(&token.lexeme, val.into())?;
                }
                Ok(Value::Nil)
            }
            Expr::Binary(inner) => inner.interpret(interpreter, environment),
            Expr::Unary(inner) => inner.interpret(interpreter, environment),

            Expr::Call(callee, paren, arguments) => {
                let mut function = match callee.interpret(interpreter, environment.clone())? {
                    Value::Callable(inner) => inner,
                    _ => {
                        eprintln!("tried to call: {:?}", callee);
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
                    args.push(arg.execute(interpreter, environment.clone())?);
                }

                function.call(interpreter, environment, args)
            }

            Expr::Logical(inner) => inner.interpret(interpreter, environment),

            Expr::Grouping(expression) => expression.interpret(interpreter, environment),
            Expr::Literal(literal) => Ok(literal.clone().into()),
            Expr::Variable(token) => {
                let lookup = if let Some(distance) = interpreter.locals.get(&token.lexeme) {
                    environment.borrow_mut().get_at(*distance, &token.lexeme)?
                } else {
                    interpreter.globals.borrow().get(&token.lexeme)?
                };

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
    fn interpret(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult {
        let left = self.left.interpret(interpreter, environment.clone())?;
        let right = self.right.interpret(interpreter, environment)?;

        Ok(match self.operator.kind {
            TokenKind::MINUS => {
                let (left, right) = get_number_operands(&self.operator, &left, &right)?;
                (left - right).into()
            }

            TokenKind::SLASH => {
                let (left, right) = get_number_operands(&self.operator, &left, &right)?;
                (left / right).into()
            }

            TokenKind::STAR => {
                let (left, right) = get_number_operands(&self.operator, &left, &right)?;
                (left * right).into()
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
                let (left, right) = get_number_operands(&self.operator, &left, &right)?;
                (left > right).into()
            }

            TokenKind::GREATER_EQUAL => {
                let (left, right) = get_number_operands(&self.operator, &left, &right)?;
                (left >= right).into()
            }

            TokenKind::LESS => {
                let (left, right) = get_number_operands(&self.operator, &left, &right)?;
                (left < right).into()
            }

            TokenKind::LESS_EQUAL => {
                let (left, right) = get_number_operands(&self.operator, &left, &right)?;
                (left <= right).into()
            }

            TokenKind::EQUAL_EQUAL => (left == right).into(),

            _ => unreachable!(),
        })
    }
}

impl Interpretable for UnaryExpr {
    fn interpret(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult {
        let right = self.expr.interpret(interpreter, environment)?;
        Ok(match self.operator.kind {
            TokenKind::MINUS => {
                let right = get_number_operand(&self.operator, &right)?;
                let val = -right;
                val.into()
            }
            TokenKind::BANG => (!right.is_truthy()).into(),
            _ => unreachable!(),
        })
    }
}

impl Interpretable for LogicalExpr {
    fn interpret(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult {
        // only evaluate left to possibly short circut
        let left = self.left.interpret(interpreter, environment.clone())?;
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
        self.right.interpret(interpreter, environment)
    }
}

impl Executable for Statement {
    fn execute(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult {
        match self {
            Statement::Expr(expr) | Statement::ForIncr(expr) => {
                expr.interpret(interpreter, environment)
            }
            Statement::If(cond, then_branch, else_branch) => {
                let out = cond.interpret(interpreter, environment.clone())?;

                if out.is_truthy() {
                    then_branch.execute(interpreter, environment)
                } else {
                    else_branch.execute(interpreter, environment)
                }
            }
            Statement::Function(name, args, body, _env) => {
                let func: Value =
                    Function::new_callable(args, body.clone(), environment.clone()).into();
                environment.borrow_mut().define(&name.lexeme, func.clone());
                Ok(func)
            }
            Statement::Print(expr) => {
                let value = expr.interpret(interpreter, environment)?;
                println!("{}", value);
                Ok(Value::Nil)
            }
            Statement::Var(token, expr) => {
                let val = expr.interpret(interpreter, environment.clone())?;
                environment.borrow_mut().define(&token.lexeme, val.into());
                Ok(Value::Nil)
            }
            Statement::While(expr, stmt) => loop {
                let out = expr.interpret(interpreter, environment.clone())?;

                if !out.is_truthy() {
                    return Ok(Value::Nil);
                }
                match stmt.execute(interpreter, environment.clone())? {
                    Value::Break => return Ok(Value::Nil),
                    Value::Return(value) => return Ok(*value),
                    _ => (),
                }
            },
            Statement::Block(block) => block.execute(interpreter, environment),
            Statement::Return(_keyword, value) => Ok(Value::Return(
                value.interpret(interpreter, environment)?.into(),
            )),
        }
    }
}

impl Executable for StatementBlock {
    fn execute(
        &self,
        interpreter: &mut Interpreter,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult {
        // make inner env our new env
        let environment = Rc::new(RefCell::new(Environment::new_enclosed(environment)));

        for stmt in &self.statements {
            match stmt.execute(interpreter, environment.clone())? {
                Value::Break => break,
                Value::Return(value) => {
                    return Ok(*value);
                }
                Value::Continue => {
                    // need to do increment in for loop if continue'd, check if last stmt is ForIncr
                    if let Statement::ForIncr(expr) = self.statements.last().unwrap() {
                        expr.interpret(interpreter, environment)?;
                    }
                    break;
                }
                _ => (),
            }
        }

        Ok(Value::Nil)
    }
}

fn get_number_operand(operator: &Token, operand: &Value) -> Result<f64, ExecutorError> {
    if let Some(num) = operand.number() {
        Ok(num)
    } else {
        Err(ExecutorError::RuntimeError(
            operator.clone(),
            "Operand must be a number.".into(),
        ))
    }
}

fn get_number_operands(
    operator: &Token,
    left: &Value,
    right: &Value,
) -> Result<(f64, f64), ExecutorError> {
    if let Some(left) = left.number() {
        if let Some(right) = right.number() {
            return Ok((left, right));
        }
    }
    Err(ExecutorError::RuntimeError(
        operator.clone(),
        "Operand must be a numbers.".into(),
    ))
}

pub struct Interpreter {
    // this might be awful
    pub stack: Vec<TokenLiteral>,
    pub globals: Rc<RefCell<Environment>>,
    pub locals: HashMap<String, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Environment::new();
        globals.define("clock", Value::Callable(Callable::Clock));

        Self {
            stack: Vec::new(),
            globals: Rc::new(globals.into()),
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Statement]) -> RuntimeResult {
        for stmt in stmts {
            stmt.execute(self, self.globals.clone())?;
        }
        Ok(Value::Nil)
    }

    pub fn resolve(&mut self, token: &Token, depth: usize) {
        self.locals.insert(token.lexeme.clone(), depth);
    }

    pub fn lookup_variable(
        &self,
        token: &Token,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult {
        if let Some(distance) = self.locals.get(&token.lexeme) {
            environment.borrow().get_at(*distance, &token.lexeme)
        } else {
            self.globals.borrow().get(&token.lexeme)
        }
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
