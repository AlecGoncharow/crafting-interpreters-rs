use crate::ast::Expr;
use crate::ast::VisitorError;
use crate::token::{Token, TokenLiteral, TokenType};
use std::collections::HashMap;

#[derive(Clone)]
pub struct Environment {
    values: HashMap<String, Expr>,
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

    pub fn define(&mut self, name: &str, val: Expr) {
        self.values.insert(name.into(), val);
    }

    pub fn assign(&mut self, name: &str, val: Expr) -> Result<(), VisitorError> {
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

    pub fn get(&self, name: &str) -> Result<&Expr, VisitorError> {
        match self.values.get(name) {
            Some(expr) => Ok(expr),
            None => {
                // try enclosing
                if let Some(inner) = &self.enclosing {
                    if let Ok(v) = inner.get(name) {
                        return Ok(v);
                    }
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
        }
    }
}
