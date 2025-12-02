use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Depth-First Search implementation.
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
}

impl<T: Eq + Hash + Clone> Default for Graph<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub fn dfs<T: Eq + Hash + Clone>(graph: &Graph<T>, start: &T) -> Vec<T> {
    let mut result = Vec::new();

    if !graph.has_vertex(start) {
        return result;
    }

    let mut visited = HashSet::new();
    let mut stack = vec![start.clone()];

    while let Some(current) = stack.pop() {
        if visited.contains(&current) {
            continue;
        }

        visited.insert(current.clone());
        result.push(current.clone());

        if let Some(neighbors) = graph.neighbors(&current) {
            for neighbor in neighbors.iter().rev() {
                if !visited.contains(neighbor) {
                    stack.push(neighbor.clone());
                }
            }
        }
    }

    result
}

pub fn dfs_recursive<T: Eq + Hash + Clone>(graph: &Graph<T>, start: &T) -> Vec<T> {
    let mut result = Vec::new();

    if !graph.has_vertex(start) {
        return result;
    }

    let mut visited = HashSet::new();
    dfs_recursive_impl(graph, start, &mut visited, &mut result);
    result
}

fn dfs_recursive_impl<T: Eq + Hash + Clone>(
    graph: &Graph<T>,
    current: &T,
    visited: &mut HashSet<T>,
    result: &mut Vec<T>,
) {
    visited.insert(current.clone());
    result.push(current.clone());

    if let Some(neighbors) = graph.neighbors(current) {
        for neighbor in neighbors {
            if !visited.contains(neighbor) {
                dfs_recursive_impl(graph, neighbor, visited, result);
            }
        }
    }
}

pub fn dfs_with_callback<T, F>(graph: &Graph<T>, start: &T, mut callback: F)
where
    T: Eq + Hash + Clone,
    F: FnMut(&T),
{
    if !graph.has_vertex(start) {
        return;
    }

    let mut visited = HashSet::new();
    let mut stack = vec![start.clone()];

    while let Some(current) = stack.pop() {
        if visited.contains(&current) {
            continue;
        }

        visited.insert(current.clone());
        callback(&current);

        if let Some(neighbors) = graph.neighbors(&current) {
            for neighbor in neighbors.iter().rev() {
                if !visited.contains(neighbor) {
                    stack.push(neighbor.clone());
                }
            }
        }
    }
}

pub fn dfs_path<T: Eq + Hash + Clone>(graph: &Graph<T>, start: &T, end: &T) -> Option<Vec<T>> {
    if !graph.has_vertex(start) || !graph.has_vertex(end) {
        return None;
    }

    if start == end {
        return Some(vec![start.clone()]);
    }

    let mut visited = HashSet::new();
    let mut path = Vec::new();

    if dfs_path_impl(graph, start, end, &mut visited, &mut path) {
        Some(path)
    } else {
        None
    }
}

fn dfs_path_impl<T: Eq + Hash + Clone>(
    graph: &Graph<T>,
    current: &T,
    end: &T,
    visited: &mut HashSet<T>,
    path: &mut Vec<T>,
) -> bool {
    visited.insert(current.clone());
    path.push(current.clone());

    if current == end {
        return true;
    }

    if let Some(neighbors) = graph.neighbors(current) {
        for neighbor in neighbors {
            if !visited.contains(neighbor) {
                if dfs_path_impl(graph, neighbor, end, visited, path) {
                    return true;
                }
            }
        }
    }

    path.pop();
    false
}

pub fn has_cycle<T: Eq + Hash + Clone>(graph: &Graph<T>, start: &T) -> bool {
    if !graph.has_vertex(start) {
        return false;
    }

    let mut visited = HashSet::new();
    let mut rec_stack = HashSet::new();

    has_cycle_impl(graph, start, &mut visited, &mut rec_stack)
}

fn has_cycle_impl<T: Eq + Hash + Clone>(
    graph: &Graph<T>,
    current: &T,
    visited: &mut HashSet<T>,
    rec_stack: &mut HashSet<T>,
) -> bool {
    visited.insert(current.clone());
    rec_stack.insert(current.clone());

    if let Some(neighbors) = graph.neighbors(current) {
        for neighbor in neighbors {
            if !visited.contains(neighbor) {
                if has_cycle_impl(graph, neighbor, visited, rec_stack) {
                    return true;
                }
            } else if rec_stack.contains(neighbor) {
                return true;
            }
        }
    }

    rec_stack.remove(current);
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_graph() {
        let graph: Graph<i32> = Graph::new();
        let result = dfs(&graph, &0);
        assert!(result.is_empty());
    }

    #[test]
    fn test_single_vertex() {
        let mut graph = Graph::new();
        graph.add_vertex(1);
        let result = dfs(&graph, &1);
        assert_eq!(result, vec![1]);
    }

    #[test]
    fn test_linear_graph() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_edge(2, 3, false);
        graph.add_edge(3, 4, false);

        let result = dfs(&graph, &1);
        assert_eq!(result.len(), 4);
        assert_eq!(result[0], 1);
    }

    #[test]
    fn test_disconnected() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_vertex(3);

        let result = dfs(&graph, &1);
        assert_eq!(result.len(), 2);
        assert!(!result.contains(&3));
    }

    #[test]
    fn test_recursive() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_edge(2, 3, false);
        graph.add_edge(3, 4, false);

        let result = dfs_recursive(&graph, &1);
        assert_eq!(result.len(), 4);
        assert_eq!(result[0], 1);
    }

    #[test]
    fn test_path_exists() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_edge(2, 3, false);
        graph.add_edge(3, 4, false);

        let path = dfs_path(&graph, &1, &4);
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

        let path = dfs_path(&graph, &1, &3);
        assert!(path.is_none());
    }

    #[test]
    fn test_has_cycle_directed() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, true);
        graph.add_edge(2, 3, true);
        graph.add_edge(3, 1, true);

        assert!(has_cycle(&graph, &1));
    }

    #[test]
    fn test_no_cycle_directed() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, true);
        graph.add_edge(2, 3, true);

        assert!(!has_cycle(&graph, &1));
    }

    #[test]
    fn test_callback() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, false);
        graph.add_edge(2, 3, false);

        let mut visited = Vec::new();
        dfs_with_callback(&graph, &1, |v| visited.push(*v));

        assert_eq!(visited.len(), 3);
        assert_eq!(visited[0], 1);
    }

    #[test]
    fn test_directed_graph() {
        let mut graph = Graph::new();
        graph.add_edge(1, 2, true);
        graph.add_edge(2, 3, true);

        let from_1 = dfs(&graph, &1);
        assert_eq!(from_1.len(), 3);

        let from_3 = dfs(&graph, &3);
        assert_eq!(from_3.len(), 1);
    }
}
