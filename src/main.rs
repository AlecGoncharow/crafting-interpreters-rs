#![feature(or_patterns)]
#![feature(nll)]

mod compiler;
mod interpreter;
mod lox;
mod vm;

use compiler::value::Value;
use lox::simple_write;
use std::env;
use std::fs;
use std::io;
use std::io::{Error, ErrorKind};
use vm::VirtualMachine;

fn repl(vm: &mut VirtualMachine) -> io::Result<()> {
    loop {
        simple_write("> ")?;

        let mut input = String::new();
        let value = match io::stdin().read_line(&mut input) {
            Ok(_) => match vm.interpret(&input) {
                Ok(v) => v,
                Err(_) => Default::default(),
            },
            Err(e) => {
                eprintln!("{}", e);
                Default::default()
            }
        };
        println!("{:?}", value);
    }
}

pub fn run_file(vm: &mut VirtualMachine, path: &str) -> io::Result<Value> {
    let bytes = fs::read(path)?;
    Ok(vm.interpret(&String::from_utf8(bytes).unwrap()).unwrap())
}

fn main() -> io::Result<()> {
    let mut vm = VirtualMachine::init();
    /*
    let constant = chunk.add_constant(1.2);
    chunk.write_op(OpCode::Constant);
    chunk.write(constant);

    chunk.write_op(OpCode::Negate);

    let constant = chunk.add_constant(3.4);
    chunk.write_op(OpCode::Constant);
    chunk.write(constant);

    chunk.write_op(OpCode::Add);

    let constant = chunk.add_constant(5.6);
    chunk.write_op(OpCode::Constant);
    chunk.write(constant);

    chunk.write_op(OpCode::Divide);

    chunk.write_op(OpCode::Return);
    */

    let use_vm = true;

    let args = env::args();
    match args.len() {
        1 => {
            if use_vm {
                repl(&mut vm)?;
            } else {
                lox::run_prompt()?;
            }
        }
        2 => {
            let path = args.last().expect("what");
            if use_vm {
                println!("{:?}", run_file(&mut vm, &path));
            } else {
                println!("{:?}", lox::run_file(&path));
            }
        }
        _ => return Err(Error::new(ErrorKind::Other, "Usage: Foo [script]")),
    };
    Ok(())
}

#[cfg(test)]
mod test_interpreter {
    use super::interpreter::value::Value;
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

    fn run_str(s: &str) -> io::Result<Value> {
        let path = make_path(s);
        Ok(lox::run_file(&path)?)
    }

    #[test]
    fn anon_fun_is_three() {
        let run = run_str("anon_fun").unwrap();
        assert_eq!(run, Value::Number(3.0));
    }

    #[test]
    fn fib_is_thirty_four() {
        let run = run_str("fib").unwrap();
        assert_eq!(run, Value::Number(34.0));
    }

    #[test]
    fn scope_not_leaked() {
        let run = run_str("leak").unwrap();
        assert_eq!(run, Value::Str("global".into()));
    }

    #[test]
    fn nest_fun_scope_capture() {
        let run = run_str("fun_closure").unwrap();
        assert_eq!(run, Value::Number(3.0));
    }

    #[test]
    fn logical_works() {
        let run = run_str("logical").unwrap();
        assert_eq!(run, Value::Number(1.0));
    }

    #[test]
    fn continue_works() {
        let run = run_str("continue_loop").unwrap();
        assert_eq!(run, Value::Number(1.0));
    }

    #[test]
    fn break_works() {
        let run = run_str("break_loop").unwrap();
        assert_eq!(run, Value::Number(5.0));
    }

    #[test]
    fn class_fields_works() {
        let run = run_str("class_fields").unwrap();
        assert_eq!(run, Value::Number(96.0));
    }

    #[test]
    fn class_methods_works() {
        let run = run_str("methods").unwrap();
        assert_eq!(run, Value::Str("it works!".into()));
    }

    #[test]
    fn class_this_works() {
        let run = run_str("class_this").unwrap();
        assert_eq!(run, Value::Number(20.0));
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
    test_ok!(anon_fun);

    test_err!(var_err);
    test_err!(mismatch_err);
}
