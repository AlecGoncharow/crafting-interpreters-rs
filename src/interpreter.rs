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

                    _ => unreachable!(),
                }
            }
            Expr::Grouping(expression) => self.execute(expression)?,
            Expr::Literal(literal) => {
                self.stack.push(literal.clone());
            }
            Expr::Variable(token) => self
                .stack
                .push(self.environment.get(&token.lexeme)?.literal().clone()),
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
            Statement::Expr(expr) => self.visit_expr(expr),
            Statement::Print(expr) => {
                self.execute(expr)?;
                let value = self.output().unwrap_or(TokenLiteral::None);
                println!("{}", value);
                Ok(())
            }
            Statement::Var(token, expr) => {
                self.execute(expr)?;
                let val = Expr::Literal(self.output().unwrap_or(TokenLiteral::None));
                self.environment.define(&token.lexeme, val);
                Ok(())
            }
            Statement::Block(stmts) => {
                self.execute_block(stmts)?;
                Ok(())
            }
        }
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
        }

        // return out env to main env
        self.environment = *self.environment.clone().into_enclosing().unwrap();

        Ok(())
    }

    pub fn output(&mut self) -> Option<TokenLiteral> {
        self.stack.pop()
    }
}
