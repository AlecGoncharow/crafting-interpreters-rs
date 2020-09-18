use crate::compiler::value::Value;

static DEFAULT_CAPACITY: usize = 256;
static MAX_LOAD: f32 = 0.75;

pub struct HashTable {
    count: usize,
    capacity: usize,

    entries: Vec<Entry>,
}

impl HashTable {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: DEFAULT_CAPACITY,
            entries: Vec::with_capacity(DEFAULT_CAPACITY),
        }
    }

    /// returns old value if exists
    pub fn insert(&mut self, key: &str, value: Value) -> Option<Value> {
        let (index, entry) = self.get_indexed(key);
        let old_value = if let Some(entry) = entry {
            Some(entry.value.clone())
        } else {
            None
        };

        if old_value.is_none() {
            self.count += 1;
        }

        self.entries[index] = Entry {
            key: String::from(key),
            value,
        };

        old_value
    }

    // FNV-1a
    pub fn index_of(&self, key: &str) -> usize {
        // @TODO should somehow pass this in, seems smart to wrap our key's in a Smart String
        // struct
        let mut index = hash_string(key) % self.capacity;

        loop {
            match self.entries.get(index) {
                Some(entry) => {
                    if entry.key == key {
                        return index;
                    }

                    index = (index + 1) % self.capacity;
                }
                None => return index,
            }
        }
    }

    pub fn get(&self, key: &str) -> Option<&Entry> {
        let index = self.index_of(key);

        self.entries.get(index)
    }

    pub fn get_indexed(&self, key: &str) -> (usize, Option<&Entry>) {
        let index = self.index_of(key);

        (index, self.entries.get(index))
    }
}

pub struct Entry {
    key: String,
    value: Value,
}

pub fn hash_string(key: &str) -> usize {
    let mut hash: usize = 2166136261;

    let bytes = key.as_bytes();
    for i in 0..key.len() {
        hash ^= bytes[i] as usize;
        hash *= 16777619;
    }

    hash
}
