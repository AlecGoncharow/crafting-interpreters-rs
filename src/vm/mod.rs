use crate::compiler;
use compiler::chunk::{Chunk, OpCode};
use compiler::compile;
use compiler::debug::disassemble_instruction;
use compiler::value::Value;

pub const STACK_MAX: usize = 256;

#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError,
}

pub type InterpretResult = Result<Value, InterpretError>;

pub struct VirtualMachine {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    stack_top: usize,
    debug: bool,
}

impl VirtualMachine {
    pub fn init() -> Self {
        Self {
            chunk: Chunk::init(),
            ip: 0,
            stack: vec![Default::default(); STACK_MAX],
            stack_top: 0,
            debug: true,
        }
    }

    pub fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    pub fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top].clone()
    }

    pub fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack_top - (1 + distance)]
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        self.chunk = if let Ok(chunk) = compile(source) {
            chunk
        } else {
            return Err(InterpretError::CompileError);
        };

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
                self.chunk.constants.values[read_byte!() as usize].clone()
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
                    if self.stack_top > 0 {
                        let pop = self.pop();
                        return Ok(pop);
                    } else {
                        return Ok(Value::Nil);
                    }
                }
                OpCode::Constant => {
                    let constant = read_constant!();
                    self.push(constant);
                }

                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),

                OpCode::Negate => {
                    if let Some(number) = self.peek(0).number() {
                        self.stack[self.stack_top - 1] = Value::Number(-number)
                    } else {
                        self.runtime_error("Operand must be a number.");
                        return Err(InterpretError::RuntimeError);
                    }
                }

                OpCode::Not => {
                    self.stack[self.stack_top - 1] =
                        self.stack[self.stack_top - 1].is_falsey().into()
                }

                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();

                    self.push(Value::Bool(a == b))
                }

                OpCode::Print => {
                    println!("{}", self.pop());
                }

                OpCode::Add
                | OpCode::Subtract
                | OpCode::Multiply
                | OpCode::Divide
                | OpCode::Greater
                | OpCode::Less => {
                    let right = self.pop();
                    let left = self.pop();
                    self.push(instruction.apply_binary(left, right).unwrap());
                }
                _ => unimplemented!(),
            }
        }
    }

    fn runtime_error(&mut self, msg: &str) {
        println!("{}", msg);

        let instruction = self.ip - 1;
        let location = self.chunk.locations[instruction];

        eprintln!("[line: {}, col: {}] in script", location.line, location.col);

        self.reset_stack();
    }

    pub fn free(self) {}
}
