use crate::token::{Token, TokenLiteral};

trait Visitor {
    fn visit_expr(&mut self, expr: &Expr);
}

trait Acceptor<T> {
    fn accept(&self, visitor: &mut T);
}

pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(TokenLiteral),
    Unary(Token, Box<Expr>),
}

impl<T: Visitor> Acceptor<T> for Expr {
    fn accept(&self, visitor: &mut T) {
        visitor.visit_expr(self);
    }
}

pub struct AstPrinter {
    buf: String,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self { buf: String::new() }
    }

    pub fn print(mut self, expr: &Expr) {
        expr.accept(&mut self);
        println!("{}", self.buf);
    }

    pub fn parenthesize(&mut self, name: &str, exprs: &[&Box<Expr>]) {
        self.buf.push('(');
        self.buf.push_str(name);
        for expr in exprs {
            self.buf.push(' ');
            expr.accept(self);
        }
        self.buf.push(')');
    }
}

impl Visitor for AstPrinter {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary(left, operator, right) => {
                self.parenthesize(&operator.lexeme, &[left, right])
            }
            Expr::Grouping(expression) => self.parenthesize("group", &[&expression]),
            Expr::Literal(literal) => self.buf.push_str(&literal.to_string()),
            Expr::Unary(operator, expr) => self.parenthesize(&operator.lexeme, &[&expr]),
        }
    }
}
