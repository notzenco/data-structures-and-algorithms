use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// A hash table implementation using open addressing with linear probing.
/// Time: O(1) average for insert, get, remove
/// Space: O(n)

const INITIAL_CAPACITY: usize = 16;
const LOAD_FACTOR: f64 = 0.75;

#[derive(Debug, Clone)]
enum Entry<K, V> {
    Empty,
    Deleted,
    Occupied(K, V),
}

#[derive(Debug)]
pub struct HashTable<K, V> {
    entries: Vec<Entry<K, V>>,
    len: usize,
    capacity: usize,
}

impl<K: Hash + Eq + Clone, V: Clone> HashTable<K, V> {
    pub fn new() -> Self {
        HashTable {
            entries: (0..INITIAL_CAPACITY).map(|_| Entry::Empty).collect(),
            len: 0,
            capacity: INITIAL_CAPACITY,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let capacity = capacity.max(INITIAL_CAPACITY);
        HashTable {
            entries: (0..capacity).map(|_| Entry::Empty).collect(),
            len: 0,
            capacity,
        }
    }

    fn hash(&self, key: &K) -> usize {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        (hasher.finish() as usize) % self.capacity
    }

    fn resize(&mut self) {
        let old_entries = std::mem::replace(
            &mut self.entries,
            (0..self.capacity * 2).map(|_| Entry::Empty).collect(),
        );
        self.capacity *= 2;
        self.len = 0;

        for entry in old_entries {
            if let Entry::Occupied(k, v) = entry {
                self.insert(k, v);
            }
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if (self.len + 1) as f64 / self.capacity as f64 > LOAD_FACTOR {
            self.resize();
        }

        let mut index = self.hash(&key);
        let start_index = index;

        loop {
            match &self.entries[index] {
                Entry::Empty | Entry::Deleted => {
                    self.entries[index] = Entry::Occupied(key, value);
                    self.len += 1;
                    return None;
                }
                Entry::Occupied(k, _) if k == &key => {
                    let old = std::mem::replace(
                        &mut self.entries[index],
                        Entry::Occupied(key, value),
                    );
                    if let Entry::Occupied(_, v) = old {
                        return Some(v);
                    }
                    return None;
                }
                _ => {
                    index = (index + 1) % self.capacity;
                    if index == start_index {
                        panic!("Hash table is full");
                    }
                }
            }
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        let mut index = self.hash(key);
        let start_index = index;

        loop {
            match &self.entries[index] {
                Entry::Empty => return None,
                Entry::Occupied(k, v) if k == key => return Some(v),
                _ => {
                    index = (index + 1) % self.capacity;
                    if index == start_index {
                        return None;
                    }
                }
            }
        }
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        let mut index = self.hash(key);
        let start_index = index;

        loop {
            match &self.entries[index] {
                Entry::Empty => return None,
                Entry::Occupied(k, _) if k == key => {
                    if let Entry::Occupied(_, v) = &mut self.entries[index] {
                        return Some(v);
                    }
                    return None;
                }
                _ => {
                    index = (index + 1) % self.capacity;
                    if index == start_index {
                        return None;
                    }
                }
            }
        }
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let mut index = self.hash(key);
        let start_index = index;

        loop {
            match &self.entries[index] {
                Entry::Empty => return None,
                Entry::Occupied(k, _) if k == key => {
                    let old = std::mem::replace(&mut self.entries[index], Entry::Deleted);
                    self.len -= 1;
                    if let Entry::Occupied(_, v) = old {
                        return Some(v);
                    }
                    return None;
                }
                _ => {
                    index = (index + 1) % self.capacity;
                    if index == start_index {
                        return None;
                    }
                }
            }
        }
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.get(key).is_some()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn clear(&mut self) {
        self.entries = (0..self.capacity).map(|_| Entry::Empty).collect();
        self.len = 0;
    }
}

impl<K: Hash + Eq + Clone, V: Clone> Default for HashTable<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_get() {
        let mut table = HashTable::new();
        table.insert("key1", "value1");
        table.insert("key2", "value2");

        assert_eq!(table.get(&"key1"), Some(&"value1"));
        assert_eq!(table.get(&"key2"), Some(&"value2"));
        assert_eq!(table.get(&"key3"), None);
    }

    #[test]
    fn test_update() {
        let mut table = HashTable::new();
        table.insert("key", "value1");
        let old = table.insert("key", "value2");

        assert_eq!(old, Some("value1"));
        assert_eq!(table.get(&"key"), Some(&"value2"));
    }

    #[test]
    fn test_remove() {
        let mut table = HashTable::new();
        table.insert("key", "value");

        assert_eq!(table.remove(&"key"), Some("value"));
        assert_eq!(table.get(&"key"), None);
        assert_eq!(table.remove(&"key"), None);
    }

    #[test]
    fn test_contains_key() {
        let mut table = HashTable::new();
        table.insert(1, "one");

        assert!(table.contains_key(&1));
        assert!(!table.contains_key(&2));
    }

    #[test]
    fn test_resize() {
        let mut table = HashTable::new();
        for i in 0..100 {
            table.insert(i, i * 2);
        }

        for i in 0..100 {
            assert_eq!(table.get(&i), Some(&(i * 2)));
        }
    }

    #[test]
    fn test_len() {
        let mut table = HashTable::new();
        assert!(table.is_empty());

        table.insert("a", 1);
        table.insert("b", 2);
        assert_eq!(table.len(), 2);

        table.remove(&"a");
        assert_eq!(table.len(), 1);
    }
}
