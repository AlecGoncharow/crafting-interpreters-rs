mod ast;
mod interpreter;
mod lox;
mod parser;
mod token;

use std::env;
use std::io;
use std::io::{Error, ErrorKind};

fn main() -> io::Result<()> {
    /*
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
    */

    for argument in env::args() {
        println!("arg :: {}", argument);
    }

    let args = env::args();
    match args.len() {
        1 => lox::run_prompt(),
        2 => {
            let path = args.last().expect("what");
            lox::run_file(&path)
        }
        _ => Err(Error::new(ErrorKind::Other, "Usage: Foo [script]")),
    }
}
