use super::value::{Value, ValueArray};
use fmt::{Display, Formatter};
use std::fmt;

#[derive(Copy, Clone)]
pub enum OpCode {
    Constant = 0,
    Return = 1,
    Negate = 2,

    Add = 3,
    Subtract = 4,
    Multiply = 5,
    Divide = 6,

    Nop = 255,
}

impl OpCode {
    pub fn apply_binary(&self, left: Value, right: Value) -> Value {
        match self {
            Self::Add => left + right,
            Self::Subtract => left - right,
            Self::Multiply => left * right,
            Self::Divide => left / right,

            _ => unreachable!(),
        }
    }
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => Self::Constant,
            1 => Self::Return,
            2 => Self::Negate,

            3 => Self::Add,
            4 => Self::Subtract,
            5 => Self::Multiply,
            6 => Self::Divide,

            255 => Self::Nop,
            _ => unimplemented!(),
        }
    }
}

impl Default for OpCode {
    fn default() -> Self {
        Self::Nop
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OpCode::Constant => write!(f, "OP_CONSTANT"),
            OpCode::Nop => write!(f, "OP_NOP"),
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
        }
    }
}

#[derive(Copy, Clone)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl Location {
    pub fn same_line(&self, other: &Location) -> bool {
        self.line == other.line
    }
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 0, col: 0 }
    }
}

#[derive(Clone)]
pub struct Chunk {
    pub count: usize,
    pub capacity: usize,

    pub code: Vec<u8>,
    pub locations: Vec<Location>,

    pub constants: ValueArray,
}

macro_rules! grow_capacity {
    ($capacity:ident) => {
        if $capacity < 8 {
            8
        } else {
            $capacity * 2
        }
    };
}

impl Chunk {
    pub fn init() -> Self {
        Self {
            count: 0,
            capacity: 0,
            code: Vec::with_capacity(0),
            locations: Vec::with_capacity(0),
            constants: ValueArray::init(),
        }
    }

    pub fn new(capacity: usize) -> Self {
        Self {
            count: 0,
            capacity,
            code: Vec::with_capacity(capacity),
            locations: Vec::with_capacity(capacity),
            constants: ValueArray::new(capacity),
        }
    }

    pub fn write(&mut self, code: u8) {
        if self.capacity < self.count + 1 {
            let old = self.capacity;
            self.capacity = grow_capacity!(old);
            self.code.resize_with(self.capacity, Default::default);
            self.locations.resize_with(self.capacity, Default::default);
        }
        self.code[self.count] = code;
        self.locations[self.count] = Default::default();
        self.count += 1;
    }

    pub fn write_op(&mut self, op: OpCode) {
        self.write(op as u8)
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.write(value);
        self.constants.count as u8 - 1
    }

    pub fn free(self) {}
}
