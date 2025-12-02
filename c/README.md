# C Implementations

Data structures and algorithms implemented in C11.

## Building

```bash
cmake -S . -B build
cmake --build build
```

## Running Tests

```bash
ctest --test-dir build --output-on-failure
```

## Structure

```
c/
├── CMakeLists.txt
├── data_structures/
│   └── stack/
│       ├── stack.h
│       ├── stack.c
│       └── stack_test.c
└── algorithms/
    ├── searching/
    │   └── binary-search/
    ├── sorting/
    │   ├── insertion-sort/
    │   ├── merge-sort/
    │   └── quick-sort/
    └── graph/
        ├── bfs/
        └── dfs/
```

## Data Structures

| Component | Status | Files |
|-----------|--------|-------|
| Stack | ✅ | [stack/](data_structures/stack/) |
| Queue | ✅ | [queue/](data_structures/queue/) |
| Dynamic Array | ✅ | [dynamic-array/](data_structures/dynamic-array/) |
| Singly Linked List | ✅ | [singly-linked-list/](data_structures/singly-linked-list/) |
| Doubly Linked List | ✅ | [doubly-linked-list/](data_structures/doubly-linked-list/) |
| Deque | ✅ | [deque/](data_structures/deque/) |
| Hash Table | ✅ | [hash-table/](data_structures/hash-table/) |
| Binary Search Tree | ✅ | [binary-search-tree/](data_structures/binary-search-tree/) |
| Binary Min Heap | ✅ | [heap-binary-min/](data_structures/heap-binary-min/) |
| Disjoint Set | ✅ | [disjoint-set/](data_structures/disjoint-set/) |

## Algorithms

### Searching

| Algorithm | Status | Files |
|-----------|--------|-------|
| Binary Search | ✅ | [binary-search/](algorithms/searching/binary-search/) |

### Sorting

| Algorithm | Status | Files |
|-----------|--------|-------|
| Insertion Sort | ✅ | [insertion-sort/](algorithms/sorting/insertion-sort/) |
| Merge Sort | ✅ | [merge-sort/](algorithms/sorting/merge-sort/) |
| Quick Sort | ✅ | [quick-sort/](algorithms/sorting/quick-sort/) |

### Graph

| Algorithm | Status | Files |
|-----------|--------|-------|
| BFS | ✅ | [bfs/](algorithms/graph/bfs/) |
| DFS | ✅ | [dfs/](algorithms/graph/dfs/) |
