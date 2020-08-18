use crate::ast::Expr;
use crate::ast::Statement;
use crate::ast::VisitorError;
use crate::token::{Token, TokenLiteral, TokenType};
use std::collections::HashMap;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Statement>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_enclosed(enclosing: Environment) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    pub fn into_enclosing(self) -> Option<Box<Self>> {
        self.enclosing
    }

    pub fn define(&mut self, name: &str, val: Statement) {
        self.values.insert(name.into(), val);
    }

    pub fn assign(&mut self, name: &str, val: Statement) -> Result<(), VisitorError> {
        match self.values.get(name) {
            Some(_) => {
                self.values.insert(name.into(), val);
                Ok(())
            }
            None => {
                if let Some(inner) = &self.enclosing {
                    let mut take_inner = inner.clone();
                    take_inner.assign(name, val)?;
                    self.enclosing = Some(take_inner);
                    Ok(())
                } else {
                    Err(VisitorError::RuntimeError(
                        Token::new(
                            TokenType::IDENTIFIER,
                            name,
                            TokenLiteral::Identifier(name.into()),
                            0,
                        ),
                        "Undefined variable.".into(),
                    ))
                }
            }
        }
    }

    pub fn get(&self, name: &str) -> Result<&Statement, VisitorError> {
        return match self.values.get(name) {
            Some(stmt) => match stmt {
                Statement::Expr(expr) => match expr {
                    Expr::Literal(TokenLiteral::Uninit) => Err(VisitorError::RuntimeError(
                        Token::none(),
                        "Variable used before initialization".into(),
                    )),
                    _ => Ok(stmt),
                },
                _ => Ok(stmt),
            },
            None => {
                // try enclosing
                if let Some(inner) = &self.enclosing {
                    return match inner.get(name) {
                        Ok(stmt) => match stmt {
                            Statement::Expr(expr) => match expr {
                                Expr::Literal(TokenLiteral::Uninit) => {
                                    Err(VisitorError::RuntimeError(
                                        Token::none(),
                                        "Variable used before initialization".into(),
                                    ))
                                }
                                _ => Ok(stmt),
                            },
                            _ => Ok(stmt),
                        },
                        Err(e) => Err(e),
                    };
                }

                Err(VisitorError::RuntimeError(
                    Token::new(
                        TokenType::IDENTIFIER,
                        name,
                        TokenLiteral::Identifier(name.into()),
                        0,
                    ),
                    "Undefined variable.".into(),
                ))
            }
        };
    }
}
