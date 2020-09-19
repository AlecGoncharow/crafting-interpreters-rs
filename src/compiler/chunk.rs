use super::value::{Object, Value, ValueArray};
use crate::vm::{InterpretError, InterpretResult};
use fmt::{Display, Formatter};
use std::fmt;

#[derive(Copy, Clone, PartialEq)]
pub enum OpCode {
    Constant = 0,
    Return = 1,
    Negate = 2,

    Add = 3,
    Subtract = 4,
    Multiply = 5,
    Divide = 6,

    Nil = 7,
    True = 8,
    False = 9,

    Not = 10,
    Equal = 11,
    Greater = 12,
    Less = 13,

    Nop = 255,
}

impl OpCode {
    pub fn apply_binary(&self, left: Value, right: Value) -> InterpretResult {
        Ok(match (left, right) {
            (Value::Obj(Object::Str(left)), Value::Obj(Object::Str(right))) => {
                Value::from(left.value + right.value.as_str())
            }

            (Value::Number(left), Value::Number(right)) => {
                if self == &Self::Greater || self == &Self::Less {
                    Value::Bool(match self {
                        Self::Greater => left > right,
                        Self::Less => left < right,

                        _ => unreachable!(),
                    })
                } else {
                    Value::Number(match self {
                        Self::Add => left + right,
                        Self::Subtract => left - right,
                        Self::Multiply => left * right,
                        Self::Divide => left / right,

                        _ => unreachable!(),
                    })
                }
            }
            _ => return Err(InterpretError::RuntimeError),
        })
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

            7 => Self::Nil,
            8 => Self::True,
            9 => Self::False,

            10 => Self::Not,
            11 => Self::Equal,
            12 => Self::Greater,
            13 => Self::Less,

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

            OpCode::Nil => write!(f, "OP_NIL"),
            OpCode::True => write!(f, "OP_TRUE"),
            OpCode::False => write!(f, "OP_FALSE"),
            Self::Not => write!(f, "OP_NOT"),
            Self::Equal => write!(f, "OP_EQ"),
            Self::Greater => write!(f, "OP_GREATER"),
            Self::Less => write!(f, "OP_LESS"),
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

    #[allow(dead_code)]
    pub fn new(capacity: usize) -> Self {
        Self {
            count: 0,
            capacity,
            code: Vec::with_capacity(capacity),
            locations: Vec::with_capacity(capacity),
            constants: ValueArray::new(capacity),
        }
    }

    pub fn write(&mut self, code: u8, location: (usize, usize)) {
        if self.capacity < self.count + 1 {
            let old = self.capacity;
            self.capacity = grow_capacity!(old);
            self.code.resize_with(self.capacity, Default::default);
            self.locations.resize_with(self.capacity, Default::default);
        }
        self.code[self.count] = code;
        self.locations[self.count] = Location {
            line: location.0,
            col: location.1,
        };
        self.count += 1;
    }

    #[allow(dead_code)]
    pub fn write_op(&mut self, op: OpCode, location: (usize, usize)) {
        self.write(op as u8, location)
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.write(value);
        self.constants.count as u8 - 1
    }

    #[allow(dead_code)]
    pub fn free(self) {}
}
