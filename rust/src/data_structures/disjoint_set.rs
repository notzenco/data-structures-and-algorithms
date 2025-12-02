/// A disjoint set (union-find) implementation with path compression and union by rank.
/// Time: O(α(n)) amortized for find/union (nearly constant)
/// Space: O(n)
#[derive(Debug, Clone)]
pub struct DisjointSet {
    parent: Vec<usize>,
    rank: Vec<usize>,
    size: Vec<usize>,
    num_sets: usize,
}

impl DisjointSet {
    pub fn new(n: usize) -> Self {
        DisjointSet {
            parent: (0..n).collect(),
            rank: vec![0; n],
            size: vec![1; n],
            num_sets: n,
        }
    }

    pub fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]);
        }
        self.parent[x]
    }

    pub fn union(&mut self, x: usize, y: usize) -> bool {
        let root_x = self.find(x);
        let root_y = self.find(y);

        if root_x == root_y {
            return false;
        }

        match self.rank[root_x].cmp(&self.rank[root_y]) {
            std::cmp::Ordering::Less => {
                self.parent[root_x] = root_y;
                self.size[root_y] += self.size[root_x];
            }
            std::cmp::Ordering::Greater => {
                self.parent[root_y] = root_x;
                self.size[root_x] += self.size[root_y];
            }
            std::cmp::Ordering::Equal => {
                self.parent[root_y] = root_x;
                self.size[root_x] += self.size[root_y];
                self.rank[root_x] += 1;
            }
        }

        self.num_sets -= 1;
        true
    }

    pub fn connected(&mut self, x: usize, y: usize) -> bool {
        self.find(x) == self.find(y)
    }

    pub fn set_size(&mut self, x: usize) -> usize {
        let root = self.find(x);
        self.size[root]
    }

    pub fn num_sets(&self) -> usize {
        self.num_sets
    }

    pub fn len(&self) -> usize {
        self.parent.len()
    }

    pub fn is_empty(&self) -> bool {
        self.parent.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_initial_state() {
        let ds = DisjointSet::new(5);
        assert_eq!(ds.num_sets(), 5);
        assert_eq!(ds.len(), 5);
    }

    #[test]
    fn test_union_find() {
        let mut ds = DisjointSet::new(5);

        assert!(ds.union(0, 1));
        assert!(ds.connected(0, 1));
        assert!(!ds.connected(0, 2));
        assert_eq!(ds.num_sets(), 4);

        assert!(ds.union(2, 3));
        assert_eq!(ds.num_sets(), 3);

        assert!(ds.union(0, 2));
        assert!(ds.connected(0, 3));
        assert!(ds.connected(1, 2));
        assert_eq!(ds.num_sets(), 2);
    }

    #[test]
    fn test_union_same_set() {
        let mut ds = DisjointSet::new(5);
        ds.union(0, 1);

        assert!(!ds.union(0, 1));
        assert!(!ds.union(1, 0));
        assert_eq!(ds.num_sets(), 4);
    }

    #[test]
    fn test_set_size() {
        let mut ds = DisjointSet::new(5);

        assert_eq!(ds.set_size(0), 1);

        ds.union(0, 1);
        assert_eq!(ds.set_size(0), 2);
        assert_eq!(ds.set_size(1), 2);

        ds.union(0, 2);
        assert_eq!(ds.set_size(0), 3);
    }

    #[test]
    fn test_large_set() {
        let mut ds = DisjointSet::new(1000);

        for i in 0..999 {
            ds.union(i, i + 1);
        }

        assert_eq!(ds.num_sets(), 1);
        assert_eq!(ds.set_size(0), 1000);
    }

    #[test]
    fn test_path_compression() {
        let mut ds = DisjointSet::new(10);

        for i in 0..9 {
            ds.union(i, i + 1);
        }

        let root = ds.find(0);
        for i in 0..10 {
            assert_eq!(ds.find(i), root);
        }
    }
}
