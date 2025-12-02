/// A singly linked list implementation.
/// Time: O(1) for push_front, O(n) for push_back, search
/// Space: O(n)

type Link<T> = Option<Box<Node<T>>>;

#[derive(Debug)]
struct Node<T> {
    value: T,
    next: Link<T>,
}

#[derive(Debug)]
pub struct SinglyLinkedList<T> {
    head: Link<T>,
    len: usize,
}

impl<T> SinglyLinkedList<T> {
    pub fn new() -> Self {
        SinglyLinkedList { head: None, len: 0 }
    }

    pub fn push_front(&mut self, value: T) {
        let new_node = Box::new(Node {
            value,
            next: self.head.take(),
        });
        self.head = Some(new_node);
        self.len += 1;
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.head.take().map(|node| {
            self.head = node.next;
            self.len -= 1;
            node.value
        })
    }

    pub fn push_back(&mut self, value: T) {
        let new_node = Box::new(Node { value, next: None });

        if self.head.is_none() {
            self.head = Some(new_node);
        } else {
            let mut current = self.head.as_mut();
            while let Some(node) = current {
                if node.next.is_none() {
                    node.next = Some(new_node);
                    break;
                }
                current = node.next.as_mut();
            }
        }
        self.len += 1;
    }

    pub fn peek_front(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.value)
    }

    pub fn peek_front_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.value)
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn clear(&mut self) {
        self.head = None;
        self.len = 0;
    }

    pub fn contains(&self, value: &T) -> bool
    where
        T: PartialEq,
    {
        let mut current = self.head.as_ref();
        while let Some(node) = current {
            if &node.value == value {
                return true;
            }
            current = node.next.as_ref();
        }
        false
    }
}

impl<T> Default for SinglyLinkedList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Drop for SinglyLinkedList<T> {
    fn drop(&mut self) {
        let mut current = self.head.take();
        while let Some(mut node) = current {
            current = node.next.take();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop_front() {
        let mut list = SinglyLinkedList::new();
        list.push_front(1);
        list.push_front(2);
        list.push_front(3);

        assert_eq!(list.pop_front(), Some(3));
        assert_eq!(list.pop_front(), Some(2));
        assert_eq!(list.pop_front(), Some(1));
        assert_eq!(list.pop_front(), None);
    }

    #[test]
    fn test_push_back() {
        let mut list = SinglyLinkedList::new();
        list.push_back(1);
        list.push_back(2);
        list.push_back(3);

        assert_eq!(list.pop_front(), Some(1));
        assert_eq!(list.pop_front(), Some(2));
    }

    #[test]
    fn test_peek() {
        let mut list = SinglyLinkedList::new();
        assert_eq!(list.peek_front(), None);

        list.push_front(42);
        assert_eq!(list.peek_front(), Some(&42));
    }

    #[test]
    fn test_contains() {
        let mut list = SinglyLinkedList::new();
        list.push_back(1);
        list.push_back(2);
        list.push_back(3);

        assert!(list.contains(&2));
        assert!(!list.contains(&5));
    }

    #[test]
    fn test_len() {
        let mut list = SinglyLinkedList::new();
        assert_eq!(list.len(), 0);

        list.push_front(1);
        list.push_front(2);
        assert_eq!(list.len(), 2);

        list.pop_front();
        assert_eq!(list.len(), 1);
    }
}
