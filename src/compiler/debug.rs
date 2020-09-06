use crate::compiler::chunk::{Chunk, OpCode};
use crate::compiler::value::Value;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.count {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    if offset > 0 && chunk.locations[offset].same_line(&chunk.locations[offset - 1]) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.locations[offset].line)
    }

    let instruction = OpCode::from(chunk.code[offset]);
    match instruction {
        OpCode::Return
        | OpCode::Nop
        | OpCode::Negate
        | OpCode::Add
        | OpCode::Subtract
        | OpCode::Multiply
        | OpCode::Divide => simple_instruction(&instruction.to_string(), offset),
        OpCode::Constant => constant_instruction(&instruction.to_string(), chunk, offset),
        _ => {
            println!("Unknown opcode {}", instruction);
            offset + 1
        }
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code[offset + 1];
    print!("{:-16} {:4} '", name, constant);
    print_value(chunk.constants.values[constant as usize]);
    println!("'");
    offset + 2
}

pub fn print_value(value: Value) {
    print!("{}", value);
}
