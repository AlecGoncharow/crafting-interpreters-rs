use crate::ast::{BinaryExpr, Expr, LogicalExpr, Statement, StatementBlock, UnaryExpr};
use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::token::Token;
use crate::token::TokenLiteral;
use std::collections::HashMap;

pub enum ResolveError {
    ScopeError(Token, String),
}

pub trait Resolvable {
    fn resolve(&self, resolver: &mut Resolver) -> Result<(), ResolveError>;
}

impl Resolvable for Expr {
    fn resolve(&self, resolver: &mut Resolver) -> Result<(), ResolveError> {
        match self {
            Expr::Assign(token, expr) => {
                unimplemented!();
            }
            Expr::Binary(inner) => inner.resolve(resolver)?,
            Expr::Unary(inner) => inner.resolve(resolver)?,

            Expr::Call(callee, _paren, arguments) => {
                callee.resolve(resolver)?;

                for expr in arguments {
                    expr.resolve(resolver)?;
                }
            }

            Expr::Logical(inner) => inner.resolve(resolver)?,

            Expr::Grouping(expression) => expression.resolve(resolver)?,
            Expr::Literal(_literal) => (),
            Expr::Variable(token) => {
                if !resolver.scopes.is_empty()
                    && *resolver
                        .scopes
                        .first()
                        .unwrap()
                        .get(&token.lexeme)
                        .unwrap_or(&true)
                        == false
                {
                    return Err(ResolveError::ScopeError(
                        token.clone(),
                        "Cannot read local variable in its own initalizer".into(),
                    ));
                }
                unimplemented!();
                // requires a reference to the interpreter, need to think about this part more
            }
        }
        Ok(())
    }
}

impl Resolvable for BinaryExpr {
    fn resolve(&self, resolver: &mut Resolver) -> Result<(), ResolveError> {
        self.left.resolve(resolver)?;
        self.right.resolve(resolver)
    }
}

impl Resolvable for UnaryExpr {
    fn resolve(&self, resolver: &mut Resolver) -> Result<(), ResolveError> {
        self.expr.resolve(resolver)
    }
}

impl Resolvable for LogicalExpr {
    fn resolve(&self, resolver: &mut Resolver) -> Result<(), ResolveError> {
        self.left.resolve(resolver)?;
        self.right.resolve(resolver)
    }
}

impl Resolvable for Statement {
    fn resolve(&self, resolver: &mut Resolver) -> Result<(), ResolveError> {
        match self {
            Statement::Expr(expr) | Statement::ForIncr(expr) => expr.resolve(resolver)?,
            Statement::If(cond, then_branch, else_branch) => {
                cond.resolve(resolver)?;
                then_branch.resolve(resolver)?;
                else_branch.resolve(resolver)?;
            }
            Statement::Function(name, args, body, _env) => {
                unimplemented!();
            }
            Statement::Print(expr) => {
                expr.resolve(resolver)?;
            }
            Statement::Var(token, expr) => {
                resolver.declare(token);
                if expr != &Expr::Literal(TokenLiteral::Uninit) {
                    expr.resolve(resolver)?;
                }
                resolver.define(token);
            }
            Statement::While(expr, stmt) => loop {
                expr.resolve(resolver)?;
                stmt.resolve(resolver)?;
            },
            Statement::Block(block) => block.resolve(resolver)?,
            Statement::Return(_keyword, value) => {
                value.resolve(resolver)?;
            }
        }
        Ok(())
    }
}

impl Resolvable for StatementBlock {
    fn resolve(&self, resolver: &mut Resolver) -> Result<(), ResolveError> {
        resolver.begin_scope();
        // make inner env our new env
        for stmt in &self.statements {
            stmt.resolve(resolver)?;
        }
        resolver.end_scope();
        Ok(())
    }
}

pub struct Resolver {
    pub scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    fn resolve(&mut self, interpreter: &mut Interpreter) -> Result<(), ResolveError> {
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        self.scopes
            .first_mut()
            .unwrap()
            .insert(name.lexeme.clone(), false);
    }

    fn define(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        self.scopes
            .first_mut()
            .unwrap()
            .insert(name.lexeme.clone(), true);
    }
}
