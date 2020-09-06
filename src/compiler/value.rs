pub type Value = f64;

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

    pub fn free(self) {}
}
