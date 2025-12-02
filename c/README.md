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
| Deque | ❌ | - |
| Hash Table | ❌ | - |
| Binary Search Tree | ❌ | - |
| Binary Min Heap | ❌ | - |
| Disjoint Set | ❌ | - |

## Algorithms

| Algorithm | Status | Files |
|-----------|--------|-------|
| Binary Search | ❌ | - |
| Insertion Sort | ❌ | - |
| Merge Sort | ❌ | - |
| Quick Sort | ❌ | - |
| BFS | ❌ | - |
| DFS | ❌ | - |
