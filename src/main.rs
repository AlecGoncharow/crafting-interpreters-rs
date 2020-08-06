mod scanner;
mod token;

use std::env;
use std::io;
use std::io::{Error, ErrorKind};

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
