use crate::token::{Token, TokenLiteral};

pub enum VisitorError {
    RuntimeError(Token, String),
}

pub type VisitorResult = Result<(), VisitorError>;

pub trait Visitor {
    #[allow(unused_variables)]
    fn visit_expr(&mut self, expr: &Expr) -> VisitorResult {
        match expr {
            Expr::Assign(token, expr) => {}
            Expr::Binary(left, operator, right) => {}
            Expr::Grouping(expression) => {}
            Expr::Literal(literal) => {}
            Expr::Unary(operator, expr) => {}
            Expr::Logical(left, operator, right) => {}
            Expr::Variable(token) => {}
        }

        Ok(())
    }

    #[allow(unused_variables)]
    fn visit_statement(&mut self, stmt: &Statement) -> VisitorResult {
        match stmt {
            Statement::Expr(expr) => self.visit_expr(expr)?,
            Statement::If(cond, then_branch, else_branch) => {}
            Statement::Print(expr) => {}
            Statement::Var(token, expr) => {}
            Statement::While(expr, stmt) => {}
            Statement::Block(stmts) => {}
        }

        Ok(())
    }
}

pub trait Acceptor {
    fn accept(&self, visitor: &mut dyn Visitor) -> VisitorResult;
}

#[derive(Clone)]
pub enum Expr {
    Assign(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(TokenLiteral),
    Unary(Token, Box<Expr>),
    Logical(Box<Expr>, Token, Box<Expr>),
    Variable(Token),
}

impl Expr {
    pub fn none() -> Self {
        Self::Literal(TokenLiteral::None)
    }

    pub fn literal(&self) -> &TokenLiteral {
        match self {
            Self::Literal(lit) => lit,
            _ => &TokenLiteral::None,
        }
    }
}

pub enum Statement {
    Expr(Expr),
    If(Expr, Box<Statement>, Box<Statement>),
    Print(Expr),
    Var(Token, Expr),
    While(Expr, Box<Statement>),
    Block(Vec<Statement>),
}

impl Statement {
    #[allow(dead_code)]
    pub fn expr(&self) -> Expr {
        match self {
            Self::Expr(expr) | Self::Print(expr) => expr.clone(),
            Self::Var(_, expr) => expr.clone(),
            _ => Expr::none(),
        }
    }
}

impl Acceptor for Statement {
    fn accept(&self, visitor: &mut dyn Visitor) -> VisitorResult {
        visitor.visit_statement(self)
    }
}

impl Acceptor for Expr {
    fn accept(&self, visitor: &mut dyn Visitor) -> VisitorResult {
        visitor.visit_expr(self)
    }
}

impl Acceptor for Box<Statement> {
    fn accept(&self, visitor: &mut dyn Visitor) -> VisitorResult {
        visitor.visit_statement(self)
    }
}

impl Acceptor for Box<Expr> {
    fn accept(&self, visitor: &mut dyn Visitor) -> VisitorResult {
        visitor.visit_expr(self)
    }
}

pub struct AstPrinter {
    buf: String,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self { buf: String::new() }
    }

    pub fn print(mut self, expr: &Statement) -> VisitorResult {
        expr.accept(&mut self)?;
        println!("{}", self.buf);
        Ok(())
    }

    pub fn parenthesize(&mut self, name: &str, exprs: &[&Expr]) -> VisitorResult {
        self.buf.push('(');
        self.buf.push_str(name);
        for expr in exprs {
            self.buf.push(' ');
            expr.accept(self)?;
        }
        self.buf.push(')');
        Ok(())
    }
}

impl Visitor for AstPrinter {
    fn visit_expr(&mut self, expr: &Expr) -> VisitorResult {
        match expr {
            Expr::Assign(token, expr) => {
                self.parenthesize("=", &[&Expr::Literal(token.literal.clone()), expr])?;
            }
            Expr::Binary(left, operator, right) | Expr::Logical(left, operator, right) => {
                self.parenthesize(&operator.lexeme, &[left, right])?
            }
            Expr::Grouping(expression) => self.parenthesize("group", &[&expression])?,
            Expr::Literal(literal) => self.buf.push_str(&literal.to_string()),
            Expr::Unary(operator, expr) => self.parenthesize(&operator.lexeme, &[&expr])?,
            Expr::Variable(token) => self.buf.push_str(&token.literal.to_string()),
        }

        Ok(())
    }

    fn visit_statement(&mut self, stmt: &Statement) -> VisitorResult {
        match stmt {
            Statement::Expr(expr) => {
                self.visit_expr(expr)?;
            }
            Statement::If(cond, then_branch, else_branch) => {
                self.buf.push_str("(if ");
                self.visit_expr(cond)?;
                self.buf.push_str(" then ");
                self.visit_statement(then_branch)?;
                self.buf.push_str(" else ");
                self.visit_statement(else_branch)?;
                self.buf.push_str(")")
            }
            Statement::Print(expr) => {
                self.parenthesize("print", &[expr])?;
            }
            Statement::Var(token, expr) => {
                self.parenthesize("=", &[&Expr::Literal(token.literal.clone()), expr])?;
            }
            Statement::While(expr, stmt) => {
                self.buf.push_str("(while ");
                self.visit_expr(expr)?;
                self.visit_statement(stmt)?;
                self.buf.push_str(")")
            }
            Statement::Block(stmts) => {
                self.buf.push_str("(block ");
                for stmt in stmts {
                    self.visit_statement(stmt)?;
                }
                self.buf.push_str(")")
            }
        }

        Ok(())
    }
}
