use crate::compiler;
use compiler::chunk::{Chunk, OpCode};
use compiler::debug::{disassemble_instruction, print_value};
use compiler::value::Value;

pub const STACK_MAX: usize = 256;

#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError,
}

pub type InterpretResult = Result<(), InterpretError>;

pub struct VirtualMachine {
    chunk: Chunk,
    ip: usize,
    stack: [Value; STACK_MAX],
    stack_top: usize,
    debug: bool,
}

impl VirtualMachine {
    pub fn init() -> Self {
        Self {
            chunk: Chunk::init(),
            ip: 0,
            stack: [0.0; STACK_MAX],
            stack_top: 0,
            debug: true,
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top]
    }

    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        self.run()
    }

    pub fn run(&mut self) -> InterpretResult {
        macro_rules! read_byte {
            () => {{
                self.ip += 1;
                self.chunk.code[self.ip - 1]
            }};
        }

        macro_rules! read_constant {
            () => {{
                self.chunk.constants.values[read_byte!() as usize]
            }};
        }

        loop {
            if self.debug {
                print!("          ");
                let mut slot = 0;
                while slot < self.stack_top {
                    print!("[ {} ]", self.stack[slot]);
                    slot += 1;
                }
                println!();

                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = OpCode::from(read_byte!());

            match instruction {
                OpCode::Return => {
                    print_value(self.pop());
                    println!();
                    return Ok(());
                }
                OpCode::Constant => {
                    let constant = read_constant!();
                    self.push(constant);
                }
                OpCode::Negate => {
                    self.stack[self.stack_top - 1] = -self.stack[self.stack_top - 1];
                }
                OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(instruction.apply_binary(left, right));
                }
                _ => unimplemented!(),
            }
        }

        Ok(())
    }

    pub fn free(self) {}
}
