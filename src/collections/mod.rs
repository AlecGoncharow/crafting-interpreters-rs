use crate::compiler::value::{ObjString, Value};
use std::num::Wrapping;

static DEFAULT_CAPACITY: usize = 256;
static MAX_LOAD: f32 = 0.75;
static GROW_FACTOR: usize = 2;

pub struct HashTable {
    count: usize,
    capacity: usize,

    entries: Vec<Option<Entry>>,
}

impl HashTable {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: DEFAULT_CAPACITY,
            entries: std::iter::repeat_with(|| None)
                .take(DEFAULT_CAPACITY)
                .collect(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            count: 0,
            capacity,
            entries: std::iter::repeat_with(|| None).take(capacity).collect(),
        }
    }

    /// returns old value if exists
    pub fn insert(&mut self, key: &ObjString, value: Value) -> Option<Value> {
        if self.count + 1 > (self.capacity as f32 * MAX_LOAD) as usize {
            let new_capcity = self.capacity * GROW_FACTOR;
            self.resize(new_capcity);
        }

        let (index, entry) = self.get_indexed(key);
        let old_value = if let Some(entry) = entry {
            Some(entry.value.clone())
        } else {
            None
        };

        if old_value.is_none() {
            self.count += 1;
        }

        self.entries[index] = Some(Entry {
            key: key.clone(),
            value,
            is_deleted: false,
        });

        old_value
    }

    pub fn insert_all(&mut self, from: &Self) {
        for i in 0..from.capacity {
            if let Some(entry) = &from.entries[i] {
                self.insert(&entry.key, entry.value.clone());
            }
        }
    }

    pub fn index_of(&self, key: &ObjString) -> usize {
        let mut index = key.hash % self.capacity;

        loop {
            match &self.entries[index] {
                Some(entry) => {
                    if entry.key.hash == key.hash && !entry.is_deleted {
                        return index;
                    }

                    index = (index + 1) % self.capacity;
                }
                None => return index,
            }
        }
    }

    pub fn index_of_insertion(&self, key: &ObjString) -> usize {
        let mut index = key.hash % self.capacity;
        let mut passed_tombstone = None;

        loop {
            match &self.entries[index] {
                Some(entry) => {
                    if entry.key.hash == key.hash && !entry.is_deleted {
                        return index;
                    }

                    // if found a tombstone, keep track of index
                    if entry.is_deleted && passed_tombstone.is_none() {
                        passed_tombstone = Some(index);
                    }

                    index = (index + 1) % self.capacity;
                }
                None => {
                    // no matching entry found, try putting in deleted bucket
                    if let Some(tombstone) = passed_tombstone {
                        return tombstone;
                    } else {
                        return index;
                    }
                }
            }
        }
    }

    pub fn get(&self, key: &ObjString) -> &Option<Entry> {
        let index = self.index_of(key);

        &self.entries[index]
    }

    pub fn get_indexed(&self, key: &ObjString) -> (usize, &Option<Entry>) {
        let index = self.index_of_insertion(key);

        (index, &self.entries[index])
    }

    pub fn delete(&mut self, key: &ObjString) -> bool {
        if self.count == 0 {
            return false;
        }

        let index = {
            let (index, entry) = self.get_indexed(key);

            if let Some(entry) = entry {
                if !entry.is_deleted {
                    Some(index)
                } else {
                    None
                }
            } else {
                None
            }
        };

        if let Some(index) = index {
            let mut entry = self.entries[index].take().unwrap();
            entry.is_deleted = true;
            self.entries[index] = Some(entry);

            true
        } else {
            false
        }
    }

    pub fn resize(&mut self, capacity: usize) {
        let mut new_table = Self::with_capacity(capacity);

        for i in 0..self.capacity {
            if let Some(entry) = &self.entries[i] {
                if !entry.is_deleted {
                    new_table.insert(&entry.key, entry.value.clone());
                }
            }
        }

        self.capacity = capacity;
        self.count = new_table.count;
        self.entries = new_table.entries;
    }
}

#[derive(Clone)]
pub struct Entry {
    pub key: ObjString,
    pub value: Value,

    is_deleted: bool,
}

// FNV-1a
pub fn hash_string(key: &str) -> usize {
    let mut hash: Wrapping<usize> = Wrapping(2166136261);

    let bytes = key.as_bytes();
    for i in 0..key.len() {
        hash ^= Wrapping(bytes[i] as usize);
        hash *= Wrapping(16777619);
    }

    hash.0
}

#[cfg(test)]
mod test_hashtable {
    use super::*;

    #[test]
    fn test_insert() {
        let mut table = HashTable::new();

        let key = ObjString::new("Foo".into());
        let value = Value::Bool(true);

        table.insert(&key, value);

        assert!(table.count == 1);
    }

    #[test]
    fn test_inserts() {
        let mut table = HashTable::new();

        let key = ObjString::new("Foo".into());
        let value = Value::Bool(true);
        table.insert(&key, value);
        let key = ObjString::new("Bar".into());
        let value = Value::Bool(true);
        table.insert(&key, value);
        let key = ObjString::new("Baz".into());
        let value = Value::Bool(true);
        table.insert(&key, value);
        let key = ObjString::new("Fizz".into());
        let value = Value::Bool(true);
        table.insert(&key, value);
        let key = ObjString::new("Buzz".into());
        let value = Value::Bool(true);
        table.insert(&key, value);
        let key = ObjString::new("FizzBuzz".into());
        let value = Value::Bool(true);
        assert!(table.insert(&key, value).is_none());

        // Test Overwrites
        let key = ObjString::new("FizzBuzz".into());
        let value = Value::Bool(true);
        assert!(table.insert(&key, value).is_some());

        let key = ObjString::new("FizzBuzz".into());
        let value = Value::Bool(true);
        table.insert(&key, value);

        let key = ObjString::new("FizzBuzz".into());
        let value = Value::Bool(true);
        table.insert(&key, value);

        let key = ObjString::new("FizzBuzz".into());
        let value = Value::Bool(true);
        table.insert(&key, value);

        assert!(table.count == 6);
    }

    #[test]
    fn test_get() {
        let mut table = HashTable::new();

        let key = ObjString::new("Foo".into());
        let value = Value::Bool(true);

        table.insert(&key, value);

        assert!(table.get(&key).is_some());
    }

    #[test]
    fn test_delete() {
        let mut table = HashTable::new();

        let key = ObjString::new("Foo".into());
        let value = Value::Bool(true);

        table.insert(&key, value);

        assert!(table.get(&key).is_some());

        assert!(table.delete(&key));

        assert!(table.get(&key).is_none());
    }
}
