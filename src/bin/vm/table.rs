use crate::value::ObjString;
use crate::value::Value;
use std::mem::swap;
use Value as V;

struct Table {
    count: usize,
    entries: Vec<Entry>,
}

#[derive(Clone)]
struct Entry {
    key: Option<ObjString>,
    value: Value,
}

impl Entry {
    fn new() -> Self {
        Self {
            key: None,
            value: V::Nil,
        }
    }
}

const TABLE_MAX_LOAD: f64 = 0.75;

fn grow_capacity(capacity: usize) -> usize {
    if capacity == 0 {
        1
    } else {
        capacity * 2
    }
}

impl Table {
    pub fn new() -> Self {
        Self {
            count: 0,
            entries: vec![],
        }
    }

    pub fn set(&mut self, key: ObjString, value: Value) -> bool {
        if self.count + 1 > (self.capacity() as f64 * TABLE_MAX_LOAD) as usize {
            self.addjust_capacity(grow_capacity(self.capacity()));
        }

        let mut entry = self.find_entry_mut(&key);
        let is_new_key = entry.key.is_none();
        let is_tombstone = entry.value == V::Bool(true);

        entry.key = Some(key);
        entry.value = value;

        if is_new_key && !is_tombstone {
            self.count += 1;
        };
        is_new_key
    }

    pub fn add_all(&mut self, from: &Table) {
        for entry in from.entries.iter() {
            if let Some(ref key) = entry.key {
                self.set(key.clone(), entry.value.clone());
            }
        }
    }

    pub fn get(&self, key: &ObjString) -> Option<&Value> {
        if self.count == 0 {
            return None;
        }

        let entry = self.find_entry(key);
        if entry.key.is_none() {
            return None;
        }

        Some(&entry.value)
    }

    pub fn delete(&mut self, key: &ObjString) -> bool {
        if self.count == 0 {
            return false;
        }

        let mut entry = self.find_entry_mut(key);
        if entry.key.is_none() {
            return false;
        }

        entry.key = None;
        entry.value = V::Bool(true); // tombstone

        true
    }

    fn find_entry_mut(&mut self, key: &ObjString) -> &mut Entry {
        let index = self.find_index(key);
        &mut self.entries[index]
    }

    fn find_entry(&self, key: &ObjString) -> &Entry {
        &self.entries[self.find_index(key)]
    }

    fn find_index(&self, key: &ObjString) -> usize {
        let mut tombstone = None;
        let mut index = key.hash as usize % self.capacity();
        loop {
            let entry = &self.entries[index];
            if entry.key.is_none() {
                if entry.value == V::Nil {
                    return tombstone.unwrap_or(index);
                } else if tombstone.is_none() {
                    tombstone = Some(index)
                }
            } else if entry.key.as_ref().filter(|k| k == &key).is_some() {
                return index;
            }
            index = (index + 1) % self.capacity();
        }
    }

    fn capacity(&self) -> usize {
        self.entries.len()
    }

    fn addjust_capacity(&mut self, capacity: usize) {
        self.count = 0;
        let mut old_entries = vec![Entry::new(); capacity];
        swap(&mut self.entries, &mut old_entries);
        for old_entry in old_entries.iter_mut() {
            if let Some(ref key) = old_entry.key {
                let dest = self.find_entry_mut(key);
                swap(&mut dest.key, &mut old_entry.key);
                swap(&mut dest.value, &mut old_entry.value);
                self.count += 1;
            }
        }
    }
}

#[cfg(test)]
mod spec {
    use super::*;

    #[test]
    fn empty_table() {
        let table = Table::new();

        assert_eq!(table.capacity(), 0);
        assert_eq!(table.count, 0);
        assert_eq!(table.get(&ObjString::new("any")), None);
    }

    #[test]
    fn single_key_operations() {
        let key = ObjString::new("1");
        let mut table = Table::new();

        assert_eq!(table.set(key.clone(), V::Number(1.)), true);
        assert_eq!(table.get(&key), Some(&V::Number(1.)));

        assert_eq!(table.set(key.clone(), V::Number(2.)), false);
        assert_eq!(table.get(&key), Some(&V::Number(2.)));

        assert_eq!(table.delete(&key), true);
        assert_eq!(table.get(&key), None);
        assert_eq!(table.delete(&key), false);

        assert_eq!(table.set(key.clone(), V::Number(3.)), true);
        assert_eq!(table.get(&key), Some(&V::Number(3.)));
    }

    #[test]
    fn multiple_keys() {
        let mut table = Table::new();
        for i in 0..1000 {
            let s = ObjString::new(&i.to_string());
            assert!(table.set(s.clone(), V::Str(s)));
        }
        for i in 0..1000 {
            let s = ObjString::new(&i.to_string());
            assert_eq!(table.get(&s), Some(&V::Str(s)));
        }

        for i in 0..1000 {
            let s = ObjString::new(&i.to_string());
            assert!(table.delete(&s));
        }
        for i in 0..1000 {
            let s = ObjString::new(&i.to_string());
            assert_eq!(table.get(&s), None);
        }

        for i in 1000..2000 {
            let s = ObjString::new(&i.to_string());
            assert!(table.set(s.clone(), V::Str(s)));
        }
        for i in 1000..2000 {
            let s = ObjString::new(&i.to_string());
            assert_eq!(table.get(&s), Some(&V::Str(s)));
        }
    }

    #[test]
    fn grows_capacity() {
        let mut table = Table::new();
        assert_eq!(table.capacity(), 0);

        table.set(ObjString::new("1"), V::Nil);
        assert_eq!(table.capacity(), 1);

        table.set(ObjString::new("2"), V::Nil);
        assert_eq!(table.capacity(), 2);

        table.set(ObjString::new("3"), V::Nil);
        assert_eq!(table.capacity(), 4);

        table.set(ObjString::new("4"), V::Nil);
        assert_eq!(table.capacity(), 8);

        table.set(ObjString::new("5"), V::Nil);
        assert_eq!(table.capacity(), 8);

        table.set(ObjString::new("6"), V::Nil);
        assert_eq!(table.capacity(), 8);

        table.set(ObjString::new("7"), V::Nil);
        assert_eq!(table.capacity(), 16);

        table.set(ObjString::new("8"), V::Nil);
        assert_eq!(table.capacity(), 16);

        for i in 0..10000 {
            let s = ObjString::new(&i.to_string());
            table.set(s.clone(), V::Str(s));
        }
        assert_eq!(table.capacity(), 16384);
    }

    #[test]
    fn add_all() {
        let mut table = Table::new();
        table.set(ObjString::new("1"), V::Number(1.));
        table.set(ObjString::new("2"), V::Number(1.));

        let mut other = Table::new();
        other.set(ObjString::new("2"), V::Number(2.));
        other.set(ObjString::new("3"), V::Number(2.));

        table.add_all(&other);

        assert_eq!(table.get(&ObjString::new("1")), Some(&V::Number(1.)));
        assert_eq!(table.get(&ObjString::new("2")), Some(&V::Number(2.)));
        assert_eq!(table.get(&ObjString::new("3")), Some(&V::Number(2.)));
    }
}
