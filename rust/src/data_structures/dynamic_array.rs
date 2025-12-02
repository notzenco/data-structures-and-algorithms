/// A generic dynamic array implementation.
/// Time: O(1) amortized for push, O(n) for insert/remove
/// Space: O(n)
#[derive(Debug, Clone)]
pub struct DynamicArray<T> {
    data: Vec<T>,
}

impl<T> DynamicArray<T> {
    pub fn new() -> Self {
        DynamicArray { data: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        DynamicArray {
            data: Vec::with_capacity(capacity),
        }
    }

    pub fn push(&mut self, value: T) {
        self.data.push(value);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.data.pop()
    }

    pub fn insert(&mut self, index: usize, value: T) {
        if index <= self.data.len() {
            self.data.insert(index, value);
        }
    }

    pub fn remove(&mut self, index: usize) -> Option<T> {
        if index < self.data.len() {
            Some(self.data.remove(index))
        } else {
            None
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.data.get_mut(index)
    }

    pub fn set(&mut self, index: usize, value: T) -> bool {
        if index < self.data.len() {
            self.data[index] = value;
            true
        } else {
            false
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }
}

impl<T> Default for DynamicArray<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> std::ops::Index<usize> for DynamicArray<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

impl<T> std::ops::IndexMut<usize> for DynamicArray<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop() {
        let mut arr = DynamicArray::new();
        arr.push(1);
        arr.push(2);
        arr.push(3);

        assert_eq!(arr.pop(), Some(3));
        assert_eq!(arr.pop(), Some(2));
        assert_eq!(arr.len(), 1);
    }

    #[test]
    fn test_insert_remove() {
        let mut arr = DynamicArray::new();
        arr.push(1);
        arr.push(3);
        arr.insert(1, 2);

        assert_eq!(arr.get(1), Some(&2));
        assert_eq!(arr.remove(1), Some(2));
        assert_eq!(arr.len(), 2);
    }

    #[test]
    fn test_index() {
        let mut arr = DynamicArray::new();
        arr.push(10);
        arr.push(20);

        assert_eq!(arr[0], 10);
        arr[0] = 100;
        assert_eq!(arr[0], 100);
    }

    #[test]
    fn test_get_set() {
        let mut arr = DynamicArray::new();
        arr.push(1);

        assert!(arr.set(0, 42));
        assert_eq!(arr.get(0), Some(&42));
        assert!(!arr.set(10, 0));
    }
}
