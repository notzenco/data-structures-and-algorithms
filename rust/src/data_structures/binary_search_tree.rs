use std::cmp::Ordering;

/// A binary search tree implementation.
/// Time: O(log n) average, O(n) worst for insert, search, delete
/// Space: O(n)

type Link<T> = Option<Box<Node<T>>>;

#[derive(Debug)]
struct Node<T> {
    value: T,
    left: Link<T>,
    right: Link<T>,
}

#[derive(Debug)]
pub struct BinarySearchTree<T> {
    root: Link<T>,
    len: usize,
}

impl<T: Ord> BinarySearchTree<T> {
    pub fn new() -> Self {
        BinarySearchTree { root: None, len: 0 }
    }

    pub fn insert(&mut self, value: T) -> bool {
        if Self::insert_node(&mut self.root, value) {
            self.len += 1;
            true
        } else {
            false
        }
    }

    fn insert_node(node: &mut Link<T>, value: T) -> bool {
        match node {
            None => {
                *node = Some(Box::new(Node {
                    value,
                    left: None,
                    right: None,
                }));
                true
            }
            Some(n) => match value.cmp(&n.value) {
                Ordering::Less => Self::insert_node(&mut n.left, value),
                Ordering::Greater => Self::insert_node(&mut n.right, value),
                Ordering::Equal => false,
            },
        }
    }

    pub fn contains(&self, value: &T) -> bool {
        Self::search_node(&self.root, value)
    }

    fn search_node(node: &Link<T>, value: &T) -> bool {
        match node {
            None => false,
            Some(n) => match value.cmp(&n.value) {
                Ordering::Less => Self::search_node(&n.left, value),
                Ordering::Greater => Self::search_node(&n.right, value),
                Ordering::Equal => true,
            },
        }
    }

    pub fn remove(&mut self, value: &T) -> bool {
        let (new_root, removed) = Self::remove_node(self.root.take(), value);
        self.root = new_root;
        if removed {
            self.len -= 1;
        }
        removed
    }

    fn remove_node(node: Link<T>, value: &T) -> (Link<T>, bool) {
        match node {
            None => (None, false),
            Some(mut n) => match value.cmp(&n.value) {
                Ordering::Less => {
                    let (new_left, removed) = Self::remove_node(n.left.take(), value);
                    n.left = new_left;
                    (Some(n), removed)
                }
                Ordering::Greater => {
                    let (new_right, removed) = Self::remove_node(n.right.take(), value);
                    n.right = new_right;
                    (Some(n), removed)
                }
                Ordering::Equal => {
                    match (n.left.take(), n.right.take()) {
                        (None, None) => (None, true),
                        (Some(left), None) => (Some(left), true),
                        (None, Some(right)) => (Some(right), true),
                        (Some(left), Some(right)) => {
                            let (new_right, min_val) = Self::remove_min(right);
                            (
                                Some(Box::new(Node {
                                    value: min_val,
                                    left: Some(left),
                                    right: new_right,
                                })),
                                true,
                            )
                        }
                    }
                }
            },
        }
    }

    fn remove_min(node: Box<Node<T>>) -> (Link<T>, T) {
        let mut n = node;
        match n.left.take() {
            None => (n.right.take(), n.value),
            Some(left) => {
                let (new_left, min_val) = Self::remove_min(left);
                n.left = new_left;
                (Some(n), min_val)
            }
        }
    }

    pub fn min(&self) -> Option<&T> {
        Self::find_min(&self.root)
    }

    fn find_min(node: &Link<T>) -> Option<&T> {
        match node {
            None => None,
            Some(n) => {
                if n.left.is_none() {
                    Some(&n.value)
                } else {
                    Self::find_min(&n.left)
                }
            }
        }
    }

    pub fn max(&self) -> Option<&T> {
        Self::find_max(&self.root)
    }

    fn find_max(node: &Link<T>) -> Option<&T> {
        match node {
            None => None,
            Some(n) => {
                if n.right.is_none() {
                    Some(&n.value)
                } else {
                    Self::find_max(&n.right)
                }
            }
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn inorder(&self) -> Vec<&T> {
        let mut result = Vec::new();
        Self::inorder_traverse(&self.root, &mut result);
        result
    }

    fn inorder_traverse<'a>(node: &'a Link<T>, result: &mut Vec<&'a T>) {
        if let Some(n) = node {
            Self::inorder_traverse(&n.left, result);
            result.push(&n.value);
            Self::inorder_traverse(&n.right, result);
        }
    }
}

impl<T: Ord> Default for BinarySearchTree<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_contains() {
        let mut bst = BinarySearchTree::new();
        assert!(bst.insert(5));
        assert!(bst.insert(3));
        assert!(bst.insert(7));
        assert!(!bst.insert(5)); // Duplicate

        assert!(bst.contains(&5));
        assert!(bst.contains(&3));
        assert!(bst.contains(&7));
        assert!(!bst.contains(&1));
    }

    #[test]
    fn test_remove() {
        let mut bst = BinarySearchTree::new();
        bst.insert(5);
        bst.insert(3);
        bst.insert(7);

        assert!(bst.remove(&3));
        assert!(!bst.contains(&3));
        assert!(!bst.remove(&3)); // Already removed
    }

    #[test]
    fn test_min_max() {
        let mut bst = BinarySearchTree::new();
        bst.insert(5);
        bst.insert(3);
        bst.insert(7);
        bst.insert(1);
        bst.insert(9);

        assert_eq!(bst.min(), Some(&1));
        assert_eq!(bst.max(), Some(&9));
    }

    #[test]
    fn test_inorder() {
        let mut bst = BinarySearchTree::new();
        bst.insert(5);
        bst.insert(3);
        bst.insert(7);
        bst.insert(1);
        bst.insert(9);

        let result: Vec<i32> = bst.inorder().iter().map(|&&x| x).collect();
        assert_eq!(result, vec![1, 3, 5, 7, 9]);
    }

    #[test]
    fn test_len() {
        let mut bst = BinarySearchTree::new();
        assert!(bst.is_empty());

        bst.insert(5);
        bst.insert(3);
        assert_eq!(bst.len(), 2);

        bst.remove(&3);
        assert_eq!(bst.len(), 1);
    }
}
