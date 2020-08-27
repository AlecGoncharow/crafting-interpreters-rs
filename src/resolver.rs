use crate::ast::{BinaryExpr, Expr, LogicalExpr, Statement, StatementBlock, UnaryExpr};
use crate::interpreter::Interpreter;
use crate::token::Token;
use crate::token::TokenLiteral;
use std::collections::HashMap;

pub enum ResolveError {
    ScopeError(Token, String),
}

pub trait Resolvable {
    fn resolve(
        &self,
        resolver: &mut Resolver,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolveError>;
}

impl Resolvable for Expr {
    fn resolve(
        &self,
        resolver: &mut Resolver,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolveError> {
        match self {
            Expr::Assign(token, expr) => {
                expr.resolve(resolver, interpreter)?;
                resolver.resolve_local(interpreter, token);
            }
            Expr::Binary(inner) => inner.resolve(resolver, interpreter)?,
            Expr::Unary(inner) => inner.resolve(resolver, interpreter)?,

            Expr::Call(callee, _paren, arguments) => {
                callee.resolve(resolver, interpreter)?;

                for expr in arguments {
                    expr.resolve(resolver, interpreter)?;
                }
            }

            Expr::Logical(inner) => inner.resolve(resolver, interpreter)?,

            Expr::Grouping(expression) => expression.resolve(resolver, interpreter)?,
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

                resolver.resolve_local(interpreter, token);
            }
        }
        Ok(())
    }
}

impl Resolvable for BinaryExpr {
    fn resolve(
        &self,
        resolver: &mut Resolver,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolveError> {
        self.left.resolve(resolver, interpreter)?;
        self.right.resolve(resolver, interpreter)
    }
}

impl Resolvable for UnaryExpr {
    fn resolve(
        &self,
        resolver: &mut Resolver,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolveError> {
        self.expr.resolve(resolver, interpreter)
    }
}

impl Resolvable for LogicalExpr {
    fn resolve(
        &self,
        resolver: &mut Resolver,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolveError> {
        self.left.resolve(resolver, interpreter)?;
        self.right.resolve(resolver, interpreter)
    }
}

impl Resolvable for Statement {
    fn resolve(
        &self,
        resolver: &mut Resolver,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolveError> {
        match self {
            Statement::Expr(expr) | Statement::ForIncr(expr) => {
                expr.resolve(resolver, interpreter)?
            }
            Statement::If(cond, then_branch, else_branch) => {
                cond.resolve(resolver, interpreter)?;
                then_branch.resolve(resolver, interpreter)?;
                else_branch.resolve(resolver, interpreter)?;
            }
            Statement::Function(name, args, body, _env) => {
                resolver.declare(name);
                resolver.define(name);
                resolver.resolve_funciton(interpreter, args, body)?;
            }
            Statement::Print(expr) => {
                expr.resolve(resolver, interpreter)?;
            }
            Statement::Var(token, expr) => {
                resolver.declare(token);
                if expr != &Expr::Literal(TokenLiteral::Uninit) {
                    expr.resolve(resolver, interpreter)?;
                }
                resolver.define(token);
            }
            Statement::While(expr, stmt) => loop {
                expr.resolve(resolver, interpreter)?;
                stmt.resolve(resolver, interpreter)?;
            },
            Statement::Block(block) => block.resolve(resolver, interpreter)?,
            Statement::Return(_keyword, value) => {
                value.resolve(resolver, interpreter)?;
            }
        }
        Ok(())
    }
}

impl Resolvable for StatementBlock {
    fn resolve(
        &self,
        resolver: &mut Resolver,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolveError> {
        resolver.begin_scope();
        // make inner env our new env
        for stmt in &self.statements {
            stmt.resolve(resolver, interpreter)?;
        }
        resolver.end_scope();
        Ok(())
    }
}

pub struct Resolver {
    pub scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    fn resolve(
        &mut self,
        interpreter: &mut Interpreter,
        statements: &Vec<Statement>,
    ) -> Result<(), ResolveError> {
        for statement in statements {
            statement.resolve(self, interpreter)?;
        }

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

    fn resolve_funciton(
        &mut self,
        interpreter: &mut Interpreter,
        params: &Vec<Token>,
        body: &StatementBlock,
    ) -> Result<(), ResolveError> {
        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }

        body.resolve(self, interpreter)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_local(&mut self, interpreter: &mut Interpreter, token: &Token) {
        for i in self.scopes.len() - 1..=0 {
            if self.scopes.get(i).unwrap().contains_key(&token.lexeme) {
                interpreter.resolve(token, self.scopes.len() - 1 - i);
            }
        }
    }
}
