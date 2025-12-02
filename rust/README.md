# Rust Implementations

Data structures and algorithms implemented in Rust (2021 edition).

## Building

```bash
cargo build
```

## Running Tests

```bash
cargo test
```

## Structure

```
rust/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── data_structures/
│   │   ├── mod.rs
│   │   ├── stack.rs
│   │   └── ...
│   └── algorithms/
│       ├── mod.rs
│       ├── binary_search.rs
│       └── ...
└── tests/
    └── tests.rs
```

## Data Structures

| Component | Status | Files |
|-----------|--------|-------|
| Stack | ✅ | [stack.rs](src/data_structures/stack.rs) |
| Queue | ✅ | [queue.rs](src/data_structures/queue.rs) |
| Dynamic Array | ✅ | [dynamic_array.rs](src/data_structures/dynamic_array.rs) |
| Singly Linked List | ✅ | [singly_linked_list.rs](src/data_structures/singly_linked_list.rs) |
| Doubly Linked List | ✅ | [doubly_linked_list.rs](src/data_structures/doubly_linked_list.rs) |
| Deque | ✅ | [deque.rs](src/data_structures/deque.rs) |
| Hash Table | ✅ | [hash_table.rs](src/data_structures/hash_table.rs) |
| Binary Search Tree | ✅ | [binary_search_tree.rs](src/data_structures/binary_search_tree.rs) |
| Binary Min Heap | ✅ | [heap.rs](src/data_structures/heap.rs) |
| Disjoint Set | ✅ | [disjoint_set.rs](src/data_structures/disjoint_set.rs) |

## Algorithms

### Searching

| Algorithm | Status | Files |
|-----------|--------|-------|
| Binary Search | ✅ | [binary_search.rs](src/algorithms/binary_search.rs) |

### Sorting

| Algorithm | Status | Files |
|-----------|--------|-------|
| Insertion Sort | ✅ | [insertion_sort.rs](src/algorithms/insertion_sort.rs) |
| Merge Sort | ✅ | [merge_sort.rs](src/algorithms/merge_sort.rs) |
| Quick Sort | ✅ | [quick_sort.rs](src/algorithms/quick_sort.rs) |

### Graph

| Algorithm | Status | Files |
|-----------|--------|-------|
| BFS | ✅ | [bfs.rs](src/algorithms/bfs.rs) |
| DFS | ✅ | [dfs.rs](src/algorithms/dfs.rs) |
