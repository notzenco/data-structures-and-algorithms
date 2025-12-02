/// A binary min heap implementation.
/// Time: O(log n) for push/pop, O(1) for peek
/// Space: O(n)
#[derive(Debug, Clone)]
pub struct MinHeap<T> {
    data: Vec<T>,
}

impl<T: Ord> MinHeap<T> {
    pub fn new() -> Self {
        MinHeap { data: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        MinHeap {
            data: Vec::with_capacity(capacity),
        }
    }

    pub fn push(&mut self, value: T) {
        self.data.push(value);
        self.sift_up(self.data.len() - 1);
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.data.is_empty() {
            return None;
        }

        let len = self.data.len();
        self.data.swap(0, len - 1);
        let result = self.data.pop();

        if !self.data.is_empty() {
            self.sift_down(0);
        }

        result
    }

    pub fn peek(&self) -> Option<&T> {
        self.data.first()
    }

    fn sift_up(&mut self, mut index: usize) {
        while index > 0 {
            let parent = (index - 1) / 2;
            if self.data[index] < self.data[parent] {
                self.data.swap(index, parent);
                index = parent;
            } else {
                break;
            }
        }
    }

    fn sift_down(&mut self, mut index: usize) {
        let len = self.data.len();
        loop {
            let left = 2 * index + 1;
            let right = 2 * index + 2;
            let mut smallest = index;

            if left < len && self.data[left] < self.data[smallest] {
                smallest = left;
            }
            if right < len && self.data[right] < self.data[smallest] {
                smallest = right;
            }

            if smallest != index {
                self.data.swap(index, smallest);
                index = smallest;
            } else {
                break;
            }
        }
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

    pub fn from_vec(vec: Vec<T>) -> Self {
        let mut heap = MinHeap { data: vec };
        let len = heap.data.len();
        for i in (0..len / 2).rev() {
            heap.sift_down(i);
        }
        heap
    }
}

impl<T: Ord> Default for MinHeap<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Ord> FromIterator<T> for MinHeap<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let vec: Vec<T> = iter.into_iter().collect();
        Self::from_vec(vec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop() {
        let mut heap = MinHeap::new();
        heap.push(5);
        heap.push(3);
        heap.push(7);
        heap.push(1);

        assert_eq!(heap.pop(), Some(1));
        assert_eq!(heap.pop(), Some(3));
        assert_eq!(heap.pop(), Some(5));
        assert_eq!(heap.pop(), Some(7));
        assert_eq!(heap.pop(), None);
    }

    #[test]
    fn test_peek() {
        let mut heap = MinHeap::new();
        assert_eq!(heap.peek(), None);

        heap.push(5);
        heap.push(3);
        assert_eq!(heap.peek(), Some(&3));
    }

    #[test]
    fn test_from_vec() {
        let heap = MinHeap::from_vec(vec![5, 3, 7, 1, 9]);
        assert_eq!(heap.peek(), Some(&1));
    }

    #[test]
    fn test_heapsort() {
        let mut heap = MinHeap::from_vec(vec![5, 2, 8, 1, 9, 3]);
        let mut sorted = Vec::new();
        while let Some(val) = heap.pop() {
            sorted.push(val);
        }
        assert_eq!(sorted, vec![1, 2, 3, 5, 8, 9]);
    }

    #[test]
    fn test_len() {
        let mut heap = MinHeap::new();
        assert!(heap.is_empty());

        heap.push(1);
        heap.push(2);
        assert_eq!(heap.len(), 2);
    }

    #[test]
    fn test_from_iterator() {
        let heap: MinHeap<i32> = vec![5, 3, 7, 1].into_iter().collect();
        assert_eq!(heap.peek(), Some(&1));
    }
}
