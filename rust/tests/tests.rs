// Integration tests for DSA library

use dsa::data_structures::*;
use dsa::algorithms::*;

#[test]
fn test_stack_integration() {
    let mut stack = stack::Stack::new();
    stack.push(1);
    stack.push(2);
    assert_eq!(stack.pop(), Some(2));
}

#[test]
fn test_queue_integration() {
    let mut queue = queue::Queue::new();
    queue.enqueue(1);
    queue.enqueue(2);
    assert_eq!(queue.dequeue(), Some(1));
}

#[test]
fn test_binary_search_integration() {
    let arr = [1, 2, 3, 4, 5];
    assert_eq!(binary_search::binary_search(&arr, &3), Some(2));
}

#[test]
fn test_sorting_integration() {
    let mut arr = [5, 2, 8, 1, 9];
    insertion_sort::insertion_sort(&mut arr);
    assert!(arr.windows(2).all(|w| w[0] <= w[1]));

    let mut arr = [5, 2, 8, 1, 9];
    merge_sort::merge_sort(&mut arr);
    assert!(arr.windows(2).all(|w| w[0] <= w[1]));

    let mut arr = [5, 2, 8, 1, 9];
    quick_sort::quick_sort(&mut arr);
    assert!(arr.windows(2).all(|w| w[0] <= w[1]));
}

#[test]
fn test_graph_algorithms_integration() {
    let mut graph = bfs::Graph::new();
    graph.add_edge(1, 2, false);
    graph.add_edge(2, 3, false);

    let result = bfs::bfs(&graph, &1);
    assert_eq!(result.len(), 3);
}
