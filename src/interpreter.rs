use crate::ast::{BinaryExpr, Expr, LogicalExpr, Statement, StatementBlock, UnaryExpr};
use crate::environment::Environment;
use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenLiteral;
use crate::value::{Callable, Class, ClassInstance, Function, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

                if let Some(distance) = interpreter.locals.get(&token) {
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
                let lookup = if let Some(distance) = interpreter.locals.get(&token) {
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
            Statement::Function(function) => {
                let func: Value = Function::new_callable(
                    function.name.lexeme.clone(),
                    &function.params,
                    function.body.clone(),
                    environment.clone(),
                )
                .into();
                environment
                    .borrow_mut()
                    .define(&function.name.lexeme, func.clone());
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
            Statement::Class(name, _methods) => {
                environment.borrow_mut().define(&name.lexeme, Value::Nil);
                let class = Value::Callable(Class::new_callable(name.lexeme.clone()));
                environment.borrow_mut().assign(&name.lexeme, class)?;
                Ok(Value::Nil)
            }
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
                Value::Break => return Ok(Value::Break),
                Value::Return(value) => {
                    return Ok(*value);
                }
                Value::Continue => {
                    // need to do increment in for loop if continue'd, check if last stmt is ForIncr
                    if let Statement::ForIncr(expr) = self.statements.last().unwrap() {
                        expr.interpret(interpreter, environment)?;
                    }
                    return Ok(Value::Continue);
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
    pub globals: Rc<RefCell<Environment>>,
    pub locals: HashMap<Token, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Environment::new();
        globals.define("clock", Value::Callable(Callable::Clock));

        Self {
            globals: Rc::new(globals.into()),
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Statement]) -> RuntimeResult {
        println!("{:?}", self.locals);
        let mut output = Value::Nil;
        for stmt in stmts {
            output = stmt.execute(self, self.globals.clone())?;
        }
        Ok(output)
    }

    pub fn resolve(&mut self, token: &Token, depth: usize) {
        self.locals.insert(token.clone(), depth);
    }

    pub fn lookup_variable(
        &self,
        token: &Token,
        environment: Rc<RefCell<Environment>>,
    ) -> RuntimeResult {
        if let Some(distance) = self.locals.get(&token) {
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
