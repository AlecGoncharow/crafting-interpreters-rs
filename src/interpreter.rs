use crate::ast::Acceptor;
use crate::ast::Visitor;
use crate::ast::VisitorError;
use crate::ast::VisitorResult;
use crate::ast::{Expr, Statement};
use crate::environment::Environment;
use crate::token::Token;
use crate::token::TokenLiteral;
use crate::token::TokenType;

pub struct Interpreter {
    // this might be awful
    stack: Vec<TokenLiteral>,
    environment: Environment,
}

impl Visitor for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> VisitorResult {
        match expr {
            Expr::Assign(token, expr) => {
                self.execute(expr)?;
                let val = Expr::Literal(self.output().unwrap_or(TokenLiteral::None));
                self.environment.assign(&token.lexeme, val)?;
            }
            Expr::Binary(left, operator, right) => {
                self.execute(left)?;
                let left = self.stack.pop().unwrap();
                self.execute(right)?;
                let right = self.stack.pop().unwrap();

                match operator.token_type {
                    TokenType::MINUS => {
                        check_number_operands(operator, &left, &right)?;
                        self.stack.push(TokenLiteral::Number(
                            left.number().unwrap() - right.number().unwrap(),
                        ));
                    }

                    TokenType::SLASH => {
                        check_number_operands(operator, &left, &right)?;
                        self.stack.push(TokenLiteral::Number(
                            left.number().unwrap() / right.number().unwrap(),
                        ));
                    }

                    TokenType::STAR => {
                        check_number_operands(operator, &left, &right)?;
                        self.stack.push(TokenLiteral::Number(
                            left.number().unwrap() * right.number().unwrap(),
                        ));
                    }

                    TokenType::PLUS => {
                        if let TokenLiteral::Str(mut left_s) = left {
                            if let TokenLiteral::Str(right_s) = right {
                                left_s.push_str(&right_s);
                                self.stack.push(TokenLiteral::Str(left_s));
                                return Ok(());
                            }
                        } else if let TokenLiteral::Number(left_n) = left {
                            if let TokenLiteral::Number(right_n) = right {
                                self.stack.push(TokenLiteral::Number(left_n + right_n));
                                return Ok(());
                            }
                        }
                        return Err(VisitorError::RuntimeError(
                            operator.clone(),
                            "Operands must be two numbers or two strings.".into(),
                        ));
                    }

                    TokenType::GREATER => {
                        check_number_operands(operator, &left, &right)?;
                        self.stack.push(TokenLiteral::Bool(
                            left.number().unwrap() > right.number().unwrap(),
                        ));
                    }

                    TokenType::GREATER_EQUAL => {
                        check_number_operands(operator, &left, &right)?;
                        self.stack.push(TokenLiteral::Bool(
                            left.number().unwrap() >= right.number().unwrap(),
                        ));
                    }

                    TokenType::LESS => {
                        check_number_operands(operator, &left, &right)?;
                        self.stack.push(TokenLiteral::Bool(
                            left.number().unwrap() < right.number().unwrap(),
                        ));
                    }

                    TokenType::LESS_EQUAL => {
                        check_number_operands(operator, &left, &right)?;
                        self.stack.push(TokenLiteral::Bool(
                            left.number().unwrap() <= right.number().unwrap(),
                        ));
                    }

                    TokenType::EQUAL_EQUAL => {
                        self.stack.push(TokenLiteral::Bool(left == right));
                    }

                    _ => unreachable!(),
                }
            }

            Expr::Logical(left, operator, right) => {
                // only evaluate left to possibly short circut
                self.execute(left)?;
                let left = self.stack.pop().unwrap();

                match operator.token_type {
                    TokenType::OR => {
                        if left.is_truthy() {
                            self.stack.push(left);
                            return Ok(());
                        }
                    }

                    TokenType::AND => {
                        if !left.is_truthy() {
                            self.stack.push(left);
                            return Ok(());
                        }
                    }

                    _ => unreachable!(),
                }

                // couldn't short circut, eval right
                self.execute(right)?;
            }

            Expr::Grouping(expression) => self.execute(expression)?,
            Expr::Literal(literal) => {
                self.stack.push(literal.clone());
            }
            Expr::Variable(token) => {
                let lookup = self.environment.get(&token.lexeme);

                match lookup {
                    Ok(v) => self.stack.push(v.literal().clone()),
                    Err(VisitorError::RuntimeError(_, msg)) => {
                        return Err(VisitorError::RuntimeError(token.clone(), msg))
                    }
                }
            }
            Expr::Unary(operator, expr) => {
                self.execute(expr)?;
                let right = self.stack.pop().unwrap();

                match operator.token_type {
                    TokenType::MINUS => {
                        check_number_operand(operator, &right)?;
                        let val = -right.number().unwrap();
                        self.stack.push(TokenLiteral::Number(val));
                    }
                    TokenType::BANG => {
                        self.stack.push(TokenLiteral::Bool(!right.is_truthy()));
                    }

                    _ => unreachable!(),
                }
            }
        }

        Ok(())
    }

    fn visit_statement(&mut self, stmt: &Statement) -> VisitorResult {
        match stmt {
            Statement::Expr(expr) | Statement::ForIncr(expr) => self.visit_expr(expr)?,
            Statement::If(cond, then_branch, else_branch) => {
                self.execute(cond)?;
                let out = self.output().unwrap();

                if out.is_truthy() {
                    self.execute(then_branch)?;
                } else {
                    self.execute(else_branch)?;
                }
            }
            Statement::Print(expr) => {
                self.execute(expr)?;
                let value = self.output().unwrap_or(TokenLiteral::None);
                println!("{}", value);
            }
            Statement::Var(token, expr) => {
                self.execute(expr)?;
                let val = Expr::Literal(self.output().unwrap_or(TokenLiteral::None));
                self.environment.define(&token.lexeme, val);
            }
            Statement::While(expr, stmt) => loop {
                self.execute(expr)?;
                let out = self.output().unwrap();

                if !out.is_truthy() {
                    break;
                }
                self.execute(stmt)?;
                match self.peek() {
                    Some(token) => match token {
                        TokenLiteral::Break => break,
                        _ => (),
                    },
                    None => (),
                }
            },
            Statement::Block(stmts) => {
                self.execute_block(stmts)?;
            }
        }
        Ok(())
    }
}

fn check_number_operand(operator: &Token, operand: &TokenLiteral) -> VisitorResult {
    if operand.number().is_some() {
        Ok(())
    } else {
        Err(VisitorError::RuntimeError(
            operator.clone(),
            "Operand must be a number.".into(),
        ))
    }
}

fn check_number_operands(
    operator: &Token,
    left: &TokenLiteral,
    right: &TokenLiteral,
) -> VisitorResult {
    if left.number().is_some() && right.number().is_some() {
        Ok(())
    } else {
        Err(VisitorError::RuntimeError(
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

    pub fn interpret(&mut self, stmts: &[Statement]) -> VisitorResult {
        for stmt in stmts {
            self.execute(stmt)?;
        }
        Ok(())
    }

    pub fn execute(&mut self, visit: &dyn Acceptor) -> VisitorResult {
        visit.accept(self)
    }

    pub fn execute_block(&mut self, stmts: &[Statement]) -> VisitorResult {
        // make inner env our new env
        self.environment = Environment::new_enclosed(self.environment.clone());

        for stmt in stmts {
            self.execute(stmt)?;
            match self.peek() {
                Some(token) => match token {
                    TokenLiteral::Break => break,
                    TokenLiteral::Continue => {
                        // need to do increment in for loop if continue'd, check if last stmt is ForIncr
                        match stmts.last().unwrap() {
                            Statement::ForIncr(expr) => self.execute(expr)?,
                            _ => (),
                        }
                        break;
                    }
                    _ => (),
                },
                None => (),
            }
        }

        // return out env to main env
        self.environment = *self.environment.clone().into_enclosing().unwrap();

        Ok(())
    }

    pub fn output(&mut self) -> Option<TokenLiteral> {
        self.stack.pop()
    }

    pub fn peek(&mut self) -> Option<&TokenLiteral> {
        self.stack.last()
    }
}
