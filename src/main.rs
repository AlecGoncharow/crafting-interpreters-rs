mod ast;
mod scanner;
mod token;

use ast::AstPrinter;
use ast::Expr;
use std::env;
use std::io;
use std::io::{Error, ErrorKind};
use token::{Token, TokenLiteral, TokenType};

pub struct Lox {
    has_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Self { has_error: false }
    }

    pub fn error(&mut self, line: usize, msg: &str) {
        self.report(line, "", msg);
    }

    pub fn report(&mut self, line: usize, whr: &str, msg: &str) {
        eprintln!("[line {}] Error {}: {}", line, whr, msg);
        self.has_error = true;
    }
}

fn main() -> io::Result<()> {
    let expr = Expr::Binary(
        Box::new(Expr::Unary(
            Token::new(TokenType::MINUS, "-", TokenLiteral::None, 1),
            Box::new(Expr::Literal(TokenLiteral::Number(123.))),
        )),
        Token::new(TokenType::STAR, "*", TokenLiteral::None, 1),
        Box::new(Expr::Grouping(Box::new(Expr::Literal(
            TokenLiteral::Number(45.67),
        )))),
    );

    let printer = AstPrinter::new();
    printer.print(&expr);

    for argument in env::args() {
        println!("arg :: {}", argument);
    }

    let lox = Lox::new();
    let args = env::args();
    match args.len() {
        1 => scanner::run_prompt(lox),
        2 => {
            let path = args.last().expect("what");
            scanner::run_file(lox, &path)
        }
        _ => Err(Error::new(ErrorKind::Other, "Usage: Foo [script]")),
    }
}
