use std::collections::VecDeque;

/// A generic queue implementation using VecDeque.
/// Time: O(1) for enqueue, dequeue, peek
/// Space: O(n)
#[derive(Debug, Clone)]
pub struct Queue<T> {
    data: VecDeque<T>,
}

impl<T> Queue<T> {
    pub fn new() -> Self {
        Queue {
            data: VecDeque::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Queue {
            data: VecDeque::with_capacity(capacity),
        }
    }

    pub fn enqueue(&mut self, value: T) {
        self.data.push_back(value);
    }

    pub fn dequeue(&mut self) -> Option<T> {
        self.data.pop_front()
    }

    pub fn peek(&self) -> Option<&T> {
        self.data.front()
    }

    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.data.front_mut()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }
}

impl<T> Default for Queue<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_queue() {
        let queue: Queue<i32> = Queue::new();
        assert!(queue.is_empty());
    }

    #[test]
    fn test_enqueue_dequeue() {
        let mut queue = Queue::new();
        queue.enqueue(1);
        queue.enqueue(2);
        queue.enqueue(3);

        assert_eq!(queue.dequeue(), Some(1));
        assert_eq!(queue.dequeue(), Some(2));
        assert_eq!(queue.dequeue(), Some(3));
        assert_eq!(queue.dequeue(), None);
    }

    #[test]
    fn test_peek() {
        let mut queue = Queue::new();
        assert_eq!(queue.peek(), None);

        queue.enqueue(42);
        assert_eq!(queue.peek(), Some(&42));
        assert_eq!(queue.len(), 1);
    }

    #[test]
    fn test_fifo_order() {
        let mut queue = Queue::new();
        for i in 0..5 {
            queue.enqueue(i);
        }
        for i in 0..5 {
            assert_eq!(queue.dequeue(), Some(i));
        }
    }
}
