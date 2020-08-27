use crate::interpreter::ExecutorError;
use crate::interpreter::Value;
use crate::token::{Token, TokenKind, TokenLiteral};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, PartialEq, Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_enclosed(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
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
                    inner.borrow_mut().assign(name, val)?;
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

    pub fn get(&self, name: &str) -> Result<Value, ExecutorError> {
        match self.values.get(name) {
            Some(val) => {
                return Ok(val.clone());
            }
            None => {
                // try enclosing
                if let Some(inner) = &self.enclosing {
                    match inner.borrow().get(name) {
                        Ok(val) => {
                            return Ok(val.clone());
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

    pub fn get_at(&self, distance: usize, name: &str) -> Result<Value, ExecutorError> {
        match self.ancestor(distance).values.get(name) {
            Some(val) => {
                return Ok(val.clone());
            }

            None => Err(ExecutorError::RuntimeError(
                Token::new(
                    TokenKind::IDENTIFIER,
                    name,
                    TokenLiteral::Identifier(name.into()),
                    0,
                ),
                "Undefined variable.".into(),
            )),
        }
    }

    //@TODO this has to return Rc<RefCell<>>
    pub fn ancestor(&self, distance: usize) -> Environment {
        let mut environment = self.clone();
        for _ in 0..=distance {
            environment = self
                .enclosing
                .as_ref()
                .unwrap()
                .clone()
                .borrow_mut()
                .clone();
        }

        environment
    }
}
