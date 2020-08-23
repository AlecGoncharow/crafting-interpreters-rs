#![feature(or_patterns)]
#![feature(nll)]

mod ast;
mod environment;
mod interpreter;
mod lox;
mod parser;
mod token;

use std::env;
use std::io;
use std::io::{Error, ErrorKind};

fn main() -> io::Result<()> {
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

#[cfg(test)]
mod test {
    use super::*;
    static PROJECT_PATH: &'static str = env!("CARGO_MANIFEST_DIR");
    static TESTS_PATH: &'static str = "/tests/";

    macro_rules! test_ok {
        ($name:ident) => {
            #[test]
            fn $name() {
                let run = run_str(&stringify!($name));
                assert!(run.is_ok());
            }
        };
    }

    macro_rules! test_err {
        ($name:ident) => {
            #[test]
            fn $name() {
                let run = run_str(&stringify!($name));
                assert!(run.is_err());
            }
        };
    }

    fn make_path(s: &str) -> String {
        PROJECT_PATH.to_owned() + TESTS_PATH + s + ".lox"
    }

    fn run_str(s: &str) -> io::Result<()> {
        let path = make_path(s);
        lox::run_file(&path)
    }

    test_ok!(empty_file);
    test_ok!(blocks);
    test_ok!(branch);
    test_ok!(expr);
    test_ok!(exprs);
    test_ok!(for_loop);
    test_ok!(while_loop);
    test_ok!(logical);
    test_ok!(long_expr);
    test_ok!(stmts);
    test_ok!(equals);
    test_ok!(break_loop);
    test_ok!(continue_loop);
    test_ok!(simple_fun);
    test_ok!(simpler_fun);
    test_ok!(fib);
    test_ok!(nest_fun);
    test_ok!(nest_fun_2);
    test_ok!(fun_closure);
    test_ok!(clock);

    test_err!(var_err);
    test_err!(mismatch_err);
}
