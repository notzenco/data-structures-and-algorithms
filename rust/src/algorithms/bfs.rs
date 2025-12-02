use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

/// Breadth-First Search implementation.
/// Time: O(V + E)
/// Space: O(V)

#[derive(Debug, Clone)]
pub struct Graph<T> {
    adj_list: HashMap<T, Vec<T>>,
}

impl<T: Eq + Hash + Clone> Graph<T> {
    pub fn new() -> Self {
        Graph {
            adj_list: HashMap::new(),
        }
    }

    pub fn add_vertex(&mut self, vertex: T) {
        self.adj_list.entry(vertex).or_insert_with(Vec::new);
    }

    pub fn add_edge(&mut self, from: T, to: T, directed: bool) {
        self.add_vertex(from.clone());
        self.add_vertex(to.clone());

        self.adj_list.get_mut(&from).unwrap().push(to.clone());
        if !directed {
            self.adj_list.get_mut(&to).unwrap().push(from);
        }
    }

    pub fn neighbors(&self, vertex: &T) -> Option<&Vec<T>> {
        self.adj_list.get(vertex)
    }

    pub fn has_vertex(&self, vertex: &T) -> bool {
        self.adj_list.contains_key(vertex)
    }

    pub fn vertices(&self) -> impl Iterator<Item = &T> {
        self.adj_list.keys()
    }
}

impl<T: Eq + Hash + Clone> Default for Graph<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub fn bfs<T: Eq + Hash + Clone>(graph: &Graph<T>, start: &T) -> Vec<T> {
    let mut result = Vec::new();

    if !graph.has_vertex(start) {
        return result;
    }

    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();

    queue.push_back(start.clone());
    visited.insert(start.clone());

    while let Some(current) = queue.pop_front() {
        result.push(current.clone());

        if let Some(neighbors) = graph.neighbors(&current) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    visited.insert(neighbor.clone());
                    queue.push_back(neighbor.clone());
                }
            }
        }
    }

    result
}

pub fn bfs_with_callback<T, F>(graph: &Graph<T>, start: &T, mut callback: F)
where
    T: Eq + Hash + Clone,
    F: FnMut(&T),
{
    if !graph.has_vertex(start) {
        return;
    }

    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();

    queue.push_back(start.clone());
    visited.insert(start.clone());

    while let Some(current) = queue.pop_front() {
        callback(&current);

        if let Some(neighbors) = graph.neighbors(&current) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    visited.insert(neighbor.clone());
                    queue.push_back(neighbor.clone());
                }
            }
        }
    }
}

pub fn bfs_path<T: Eq + Hash + Clone>(graph: &Graph<T>, start: &T, end: &T) -> Option<Vec<T>> {
    if !graph.has_vertex(start) || !graph.has_vertex(end) {
        return None;
    }

    if start == end {
        return Some(vec![start.clone()]);
    }

    let mut visited = HashSet::new();
    let mut parent: HashMap<T, T> = HashMap::new();
    let mut queue = VecDeque::new();

    queue.push_back(start.clone());
    visited.insert(start.clone());

    while let Some(current) = queue.pop_front() {
        if &current == end {
            let mut path = Vec::new();
            let mut node = end.clone();
            while &node != start {
                path.push(node.clone());
                node = parent.get(&node).unwrap().clone();
            }
            path.push(start.clone());
            path.reverse();
            return Some(path);
        }

        if let Some(neighbors) = graph.neighbors(&current) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    visited.insert(neighbor.clone());
                    parent.insert(neighbor.clone(), current.clone());
                    queue.push_back(neighbor.clone());
                }
            }
        }
    }

    None
}

pub fn bfs_distances<T: Eq + Hash + Clone>(graph: &Graph<T>, start: &T) -> HashMap<T, usize> {
    let mut distances = HashMap::new();

    if !graph.has_vertex(start) {
        return distances;
    }

    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();

    queue.push_back((start.clone(), 0));
    visited.insert(start.clone());
    distances.insert(start.clone(), 0);

    while let Some((current, dist)) = queue.pop_front() {
        if let Some(neighbors) = graph.neighbors(&current) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    visited.insert(neighbor.clone());
                    distances.insert(neighbor.clone(), dist + 1);
                    queue.push_back((neighbor.clone(), dist + 1));
                }
            }
        }
    }

    distances
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_graph() {
        let graph: Graph<i32> = Graph::new();
        let result = bfs(&graph, &0);
        assert!(result.is_empty());
    }

    #[test]
    fn test_single_vertex() {
        let mut graph = Graph::new();
        graph.add_vertex(1);
        let result = bfs(&graph, &1);
        assert_eq!(result, vec![1]);
    }

    #[test]
    fn test_linear_graph() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_edge(2, 3, false);
        graph.add_edge(3, 4, false);

        let result = bfs(&graph, &1);
        assert_eq!(result.len(), 4);
        assert_eq!(result[0], 1);
    }

    #[test]
    fn test_disconnected() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_vertex(3);

        let result = bfs(&graph, &1);
        assert_eq!(result.len(), 2);
        assert!(!result.contains(&3));
    }

    #[test]
    fn test_path_exists() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_edge(2, 3, false);
        graph.add_edge(3, 4, false);

        let path = bfs_path(&graph, &1, &4);
        assert!(path.is_some());
        let path = path.unwrap();
        assert_eq!(path.first(), Some(&1));
        assert_eq!(path.last(), Some(&4));
    }

    #[test]
    fn test_path_not_exists() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_vertex(3);

        let path = bfs_path(&graph, &1, &3);
        assert!(path.is_none());
    }

    #[test]
    fn test_distances() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_edge(1, 3, false);
        graph.add_edge(2, 4, false);
        graph.add_edge(3, 4, false);
        graph.add_edge(4, 5, false);

        let distances = bfs_distances(&graph, &1);
        assert_eq!(distances.get(&1), Some(&0));
        assert_eq!(distances.get(&2), Some(&1));
        assert_eq!(distances.get(&3), Some(&1));
        assert_eq!(distances.get(&4), Some(&2));
        assert_eq!(distances.get(&5), Some(&3));
    }

    #[test]
    fn test_callback() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_edge(2, 3, false);

        let mut visited = Vec::new();
        bfs_with_callback(&graph, &1, |v| visited.push(*v));

        assert_eq!(visited.len(), 3);
        assert_eq!(visited[0], 1);
    }

    #[test]
    fn test_directed_graph() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, true);
        graph.add_edge(2, 3, true);

        let from_1 = bfs(&graph, &1);
        assert_eq!(from_1.len(), 3);

        let from_3 = bfs(&graph, &3);
        assert_eq!(from_3.len(), 1);
    }
}
