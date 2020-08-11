use crate::ast::Expr;
use crate::ast::VisitorError;
use crate::token::{Token, TokenLiteral, TokenType};
use std::collections::HashMap;

pub struct Environment {
    values: HashMap<String, Expr>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, val: Expr) {
        self.values.insert(name.into(), val);
    }

    pub fn get(&self, name: &str) -> Result<&Expr, VisitorError> {
        match self.values.get(name) {
            Some(expr) => Ok(expr),
            None => Err(VisitorError::RuntimeError(
                Token::new(
                    TokenType::IDENTIFIER,
                    name,
                    TokenLiteral::Identifier(name.into()),
                    0,
                ),
                "Undefined variable.".into(),
            )),
        }
    }
}
