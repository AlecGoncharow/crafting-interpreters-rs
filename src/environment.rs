use crate::interpreter::ExecutorError;
use crate::interpreter::Value;
use crate::token::{Token, TokenKind, TokenLiteral};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
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

    pub fn define(&mut self, name: &str, val: Value) {
        self.values.insert(name.into(), val);
    }

    pub fn assign(&mut self, name: &str, val: Value) -> Result<(), ExecutorError> {
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

    pub fn get(&self, name: &str) -> Result<&Value, ExecutorError> {
        match self.values.get(name) {
            Some(val) => {
                //@TODO uninitialzied vars should error
                return Ok(val);
            }
            None => {
                // try enclosing
                if let Some(inner) = &self.enclosing {
                    match inner.get(name) {
                        Ok(val) => {
                            return Ok(val);
                        }
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
