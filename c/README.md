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
    └── ...
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
| Hash Table | ✅ | [hash-table-open-addressing/](data_structures/hash-table-open-addressing/) |
| Binary Search Tree | ✅ | [binary-search-tree/](data_structures/binary-search-tree/) |
| Binary Min Heap | ✅ | [heap-binary-min/](data_structures/heap-binary-min/) |
| Disjoint Set | ✅ | [disjoint-set/](data_structures/disjoint-set/) |

## Algorithms

| Algorithm | Status | Files |
|-----------|--------|-------|
| Binary Search | ✅ | [binary-search/](algorithms/binary-search/) |
| Insertion Sort | ✅ | [insertion-sort/](algorithms/insertion-sort/) |
| Merge Sort | ✅ | [merge-sort/](algorithms/merge-sort/) |
| Quick Sort | ✅ | [quick-sort/](algorithms/quick-sort/) |
| BFS | ❌ | - |
| DFS | ❌ | - |
