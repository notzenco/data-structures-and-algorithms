# Python Implementations

Data structures and algorithms implemented in Python 3.10+.

## Installation

```bash
pip install -e .
```

## Running Tests

```bash
pip install pytest
pytest
```

## Structure

```
python/
├── pyproject.toml
├── src/
│   └── dsa/
│       ├── __init__.py
│       ├── data_structures/
│       │   ├── __init__.py
│       │   ├── stack.py
│       │   └── ...
│       └── algorithms/
│           ├── __init__.py
│           ├── binary_search.py
│           └── ...
└── tests/
    ├── test_data_structures.py
    └── test_algorithms.py
```

## Data Structures

| Component | Status | Files |
|-----------|--------|-------|
| Stack | ✅ | [stack.py](src/dsa/data_structures/stack.py) |
| Queue | ✅ | [queue.py](src/dsa/data_structures/queue.py) |
| Dynamic Array | ✅ | [dynamic_array.py](src/dsa/data_structures/dynamic_array.py) |
| Singly Linked List | ✅ | [singly_linked_list.py](src/dsa/data_structures/singly_linked_list.py) |
| Doubly Linked List | ✅ | [doubly_linked_list.py](src/dsa/data_structures/doubly_linked_list.py) |
| Deque | ✅ | [deque.py](src/dsa/data_structures/deque.py) |
| Hash Table | ✅ | [hash_table.py](src/dsa/data_structures/hash_table.py) |
| Binary Search Tree | ✅ | [binary_search_tree.py](src/dsa/data_structures/binary_search_tree.py) |
| Binary Min Heap | ✅ | [heap.py](src/dsa/data_structures/heap.py) |
| Disjoint Set | ✅ | [disjoint_set.py](src/dsa/data_structures/disjoint_set.py) |

## Algorithms

### Searching

| Algorithm | Status | Files |
|-----------|--------|-------|
| Binary Search | ✅ | [binary_search.py](src/dsa/algorithms/binary_search.py) |

### Sorting

| Algorithm | Status | Files |
|-----------|--------|-------|
| Insertion Sort | ✅ | [insertion_sort.py](src/dsa/algorithms/insertion_sort.py) |
| Merge Sort | ✅ | [merge_sort.py](src/dsa/algorithms/merge_sort.py) |
| Quick Sort | ✅ | [quick_sort.py](src/dsa/algorithms/quick_sort.py) |

### Graph

| Algorithm | Status | Files |
|-----------|--------|-------|
| BFS | ✅ | [bfs.py](src/dsa/algorithms/bfs.py) |
| DFS | ✅ | [dfs.py](src/dsa/algorithms/dfs.py) |
