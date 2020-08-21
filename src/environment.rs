use crate::ast::Expr;
use crate::ast::Statement;
use crate::interpreter::ExecutorError;
use crate::token::{Token, TokenKind, TokenLiteral};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Environment {
    values: HashMap<String, Statement>,
    pub enclosing: Option<Box<Environment>>,
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

    pub fn assign(&mut self, name: &str, val: Statement) -> Result<(), ExecutorError> {
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
                    Err(ExecutorError::RuntimeError(
                        Token::new(
                            TokenKind::IDENTIFIER,
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

    pub fn get(&self, name: &str) -> Result<&Statement, ExecutorError> {
        match self.values.get(name) {
            Some(stmt) => match stmt {
                Statement::Expr(expr) => match expr {
                    Expr::Literal(TokenLiteral::Uninit) => Err(ExecutorError::RuntimeError(
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
                    match inner.get(name) {
                        Ok(stmt) => match stmt {
                            Statement::Expr(expr) => match expr {
                                Expr::Literal(TokenLiteral::Uninit) => {
                                    return Err(ExecutorError::RuntimeError(
                                        Token::none(),
                                        "Variable used before initialization".into(),
                                    ))
                                }
                                _ => return Ok(stmt),
                            },
                            _ => return Ok(stmt),
                        },
                        Err(e) => return Err(e),
                    }
                }

                Err(ExecutorError::RuntimeError(
                    Token::new(
                        TokenKind::IDENTIFIER,
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
