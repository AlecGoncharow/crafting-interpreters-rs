use crate::token::{Token, TokenLiteral};

pub enum VisitorError {
    RuntimeError(Token, String),
}

pub type VisitorResult = Result<(), VisitorError>;

pub trait Visitor {
    #[allow(unused_variables)]
    fn visit_expr(&mut self, expr: &Expr) -> VisitorResult {
        match expr {
            Expr::Binary(left, operator, right) => {}
            Expr::Grouping(expression) => {}
            Expr::Literal(literal) => {}
            Expr::Unary(operator, expr) => {}
        }

        Ok(())
    }
}

pub trait Acceptor<T> {
    fn accept(&self, visitor: &mut T) -> VisitorResult;
}

pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(TokenLiteral),
    Unary(Token, Box<Expr>),
}

impl<T: Visitor> Acceptor<T> for Expr {
    fn accept(&self, visitor: &mut T) -> VisitorResult {
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

    pub fn print(mut self, expr: &Expr) -> VisitorResult {
        expr.accept(&mut self)?;
        println!("{}", self.buf);
        Ok(())
    }

    pub fn parenthesize(&mut self, name: &str, exprs: &[&Box<Expr>]) -> VisitorResult {
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
            Expr::Binary(left, operator, right) => {
                self.parenthesize(&operator.lexeme, &[left, right])?
            }
            Expr::Grouping(expression) => self.parenthesize("group", &[&expression])?,
            Expr::Literal(literal) => self.buf.push_str(&literal.to_string()),
            Expr::Unary(operator, expr) => self.parenthesize(&operator.lexeme, &[&expr])?,
        }

        Ok(())
    }
}
