use std::collections::VecDeque;

/// A double-ended queue implementation.
/// Time: O(1) for push/pop front/back
/// Space: O(n)
#[derive(Debug, Clone)]
pub struct Deque<T> {
    data: VecDeque<T>,
}

impl<T> Deque<T> {
    pub fn new() -> Self {
        Deque {
            data: VecDeque::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Deque {
            data: VecDeque::with_capacity(capacity),
        }
    }

    pub fn push_front(&mut self, value: T) {
        self.data.push_front(value);
    }

    pub fn push_back(&mut self, value: T) {
        self.data.push_back(value);
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.data.pop_front()
    }

    pub fn pop_back(&mut self) -> Option<T> {
        self.data.pop_back()
    }

    pub fn peek_front(&self) -> Option<&T> {
        self.data.front()
    }

    pub fn peek_back(&self) -> Option<&T> {
        self.data.back()
    }

    pub fn peek_front_mut(&mut self) -> Option<&mut T> {
        self.data.front_mut()
    }

    pub fn peek_back_mut(&mut self) -> Option<&mut T> {
        self.data.back_mut()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(index)
    }
}

impl<T> Default for Deque<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop_front() {
        let mut deque = Deque::new();
        deque.push_front(1);
        deque.push_front(2);

        assert_eq!(deque.pop_front(), Some(2));
        assert_eq!(deque.pop_front(), Some(1));
    }

    #[test]
    fn test_push_pop_back() {
        let mut deque = Deque::new();
        deque.push_back(1);
        deque.push_back(2);

        assert_eq!(deque.pop_back(), Some(2));
        assert_eq!(deque.pop_back(), Some(1));
    }

    #[test]
    fn test_mixed_operations() {
        let mut deque = Deque::new();
        deque.push_back(1);
        deque.push_front(0);
        deque.push_back(2);

        assert_eq!(deque.pop_front(), Some(0));
        assert_eq!(deque.pop_back(), Some(2));
        assert_eq!(deque.pop_front(), Some(1));
    }

    #[test]
    fn test_peek() {
        let mut deque = Deque::new();
        deque.push_back(1);
        deque.push_back(2);

        assert_eq!(deque.peek_front(), Some(&1));
        assert_eq!(deque.peek_back(), Some(&2));
    }

    #[test]
    fn test_get() {
        let mut deque = Deque::new();
        deque.push_back(10);
        deque.push_back(20);
        deque.push_back(30);

        assert_eq!(deque.get(0), Some(&10));
        assert_eq!(deque.get(1), Some(&20));
        assert_eq!(deque.get(2), Some(&30));
        assert_eq!(deque.get(3), None);
    }
}
