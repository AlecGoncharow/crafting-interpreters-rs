use crate::ast::{BinaryExpr, Expr, LogicalExpr, Statement, StatementBlock, UnaryExpr};
use crate::environment::Environment;
use crate::interpreter::{Callable, Executable, Interpretable, Interpreter, Value};
use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenLiteral;
use std::collections::HashMap;

pub trait Resolvable {
    fn resolve(&self, resolver: &mut Resolver);
}

impl Resolvable for Expr {
    fn resolve(&self, resolver: &mut Resolver) {
        match self {
            Expr::Assign(token, expr) => {
                unimplemented!();
            }
            Expr::Binary(inner) => inner.resolve(resolver),
            Expr::Unary(inner) => inner.resolve(resolver),

            Expr::Call(callee, paren, arguments) => {
                unimplemented!();
            }

            Expr::Logical(inner) => inner.resolve(resolver),

            Expr::Grouping(expression) => expression.resolve(resolver),
            Expr::Literal(literal) => unimplemented!(),
            Expr::Variable(token) => {
                unimplemented!();
            }
        }
    }
}

impl Resolvable for BinaryExpr {
    fn resolve(&self, resolver: &mut Resolver) {
        unimplemented!();
    }
}

impl Resolvable for UnaryExpr {
    fn resolve(&self, resolver: &mut Resolver) {
        unimplemented!();
        match self.operator.kind {
            TokenKind::MINUS => unimplemented!(),
            TokenKind::BANG => unimplemented!(),
            _ => unreachable!(),
        }
    }
}

impl Resolvable for LogicalExpr {
    fn resolve(&self, resolver: &mut Resolver) {
        match self.operator.kind {
            TokenKind::OR => {
                unimplemented!();
            }

            TokenKind::AND => {
                unimplemented!();
            }

            _ => unreachable!(),
        }
    }
}

impl Resolvable for Statement {
    fn resolve(&self, resolver: &mut Resolver) {
        unimplemented!();
        match self {
            Statement::Expr(expr) | Statement::ForIncr(expr) => expr.resolve(resolver),
            Statement::If(cond, then_branch, else_branch) => {
                unimplemented!();
            }
            Statement::Function(name, args, body, _env) => {
                unimplemented!();
            }
            Statement::Print(expr) => {
                unimplemented!();
            }
            Statement::Var(token, expr) => {
                unimplemented!();
            }
            Statement::While(expr, stmt) => loop {
                unimplemented!();
            },
            Statement::Block(block) => block.resolve(resolver),
            Statement::Return(_keyword, value) => {
                unimplemented!();
            }
        }
    }
}

impl Resolvable for StatementBlock {
    fn resolve(&self, resolver: &mut Resolver) {
        // make inner env our new env
        for stmt in &self.statements {
            unimplemented!();
        }
    }
}

pub struct Resolver {
    pub scopes: Vec<HashMap<String, bool>>,
}
