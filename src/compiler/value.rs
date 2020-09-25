use crate::collections::hash_string;
use std::fmt;

#[derive(Clone, Debug)]
pub struct ObjString {
    pub value: String,
    pub hash: usize,
}

impl ObjString {
    pub fn new(value: String) -> Self {
        Self {
            hash: hash_string(&value),
            value,
        }
    }
}

impl From<String> for ObjString {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl fmt::Display for ObjString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub enum Object {
    Str(ObjString),
}

impl Object {
    pub fn string(s: String) -> Self {
        Self::Str(ObjString::new(s))
    }
}

impl std::cmp::PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Str(l), Object::Str(r)) => l.hash == r.hash,
            //_ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Obj(Object),
    Nil,
}

impl Value {
    pub fn number(&self) -> Option<f64> {
        if let Self::Number(inner) = self {
            Some(*inner)
        } else {
            None
        }
    }

    pub fn string(&self) -> Option<&ObjString> {
        if let Self::Obj(Object::Str(inner)) = self {
            Some(inner)
        } else {
            None
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            Self::Nil => false,
            _ => true,
        }
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Self::Bool(b) => !*b,
            Self::Nil => true,
            _ => false,
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            (Value::Obj(l), Value::Obj(r)) => l == r,

            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            //Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Number(n) => {
                let mut s = n.to_string();
                if s.ends_with(".0") {
                    s.truncate(s.len() - 2);
                }

                write!(f, "{}", s)
            }
            Self::Bool(b) => write!(f, "{}", b.to_string()),
            Self::Nil => write!(f, "nil"),
            Self::Obj(Object::Str(s)) => write!(f, "Obj::Str {}", s),
            //Self::Obj(_) => write!(f, "Obj"),
            //Self::Uninit => write!(f, "uninit"),
            //Self::Break => write!(f, "break"),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Number(v.into())
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Bool(v.into())
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Self::Obj(Object::string(s.into()))
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Self::Obj(Object::string(s))
    }
}

#[derive(Clone)]
pub struct ValueArray {
    pub capacity: usize,
    pub count: usize,
    pub values: Vec<Value>,
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

impl ValueArray {
    pub fn init() -> Self {
        Self {
            count: 0,
            capacity: 0,
            values: Vec::with_capacity(0),
        }
    }

    pub fn new(capacity: usize) -> Self {
        Self {
            count: 0,
            capacity,
            values: Vec::with_capacity(capacity),
        }
    }

    pub fn write(&mut self, values: Value) {
        if self.capacity < self.count + 1 {
            let old = self.capacity;
            self.capacity = grow_capacity!(old);
            self.values.resize_with(self.capacity, Default::default);
        }
        self.values[self.count] = values;
        self.count += 1;
    }
}
