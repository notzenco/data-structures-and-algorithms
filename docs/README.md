# Data Structures & Algorithms - Theory and Documentation

This folder contains language-agnostic explanations of the data structures and algorithms implemented in this repository. Each document covers the mathematical foundations, complexity analysis, use cases, and trade-offs.

## Data Structures

| Structure | Description | Time Complexity (Average) |
|-----------|-------------|---------------------------|
| [Dynamic Array](data_structures/dynamic-array.md) | Resizable array with amortized O(1) append | Access: O(1), Insert: O(n) |
| [Singly Linked List](data_structures/singly-linked-list.md) | Linear collection with O(1) head insertion | Access: O(n), Insert: O(1) |
| [Doubly Linked List](data_structures/doubly-linked-list.md) | Bidirectional traversal linked list | Access: O(n), Insert: O(1) |
| [Stack](data_structures/stack.md) | LIFO container | Push/Pop: O(1) |
| [Queue](data_structures/queue.md) | FIFO container | Enqueue/Dequeue: O(1) |
| [Deque](data_structures/deque.md) | Double-ended queue | Insert/Remove: O(1) |
| [Hash Table](data_structures/hash-table.md) | Key-value store with open addressing | Insert/Search: O(1) |
| [Binary Search Tree](data_structures/binary-search-tree.md) | Ordered tree structure | Insert/Search: O(log n) |
| [Binary Min Heap](data_structures/heap.md) | Priority queue implementation | Insert: O(log n), Min: O(1) |
| [Disjoint Set](data_structures/disjoint-set.md) | Union-Find with path compression | Union/Find: O(α(n)) |

## Algorithms

### Searching

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| [Binary Search](algorithms/searching/binary-search.md) | Divide and conquer search on sorted data | O(log n) |

### Sorting

| Algorithm | Description | Time Complexity (Average) |
|-----------|-------------|---------------------------|
| [Insertion Sort](algorithms/sorting/insertion-sort.md) | Simple comparison sort | O(n²) |
| [Merge Sort](algorithms/sorting/merge-sort.md) | Stable divide-and-conquer sort | O(n log n) |
| [Quick Sort](algorithms/sorting/quick-sort.md) | In-place divide-and-conquer sort | O(n log n) |

### Graph Traversal

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| [Breadth-First Search](algorithms/graph/bfs.md) | Level-order traversal | O(V + E) |
| [Depth-First Search](algorithms/graph/dfs.md) | Recursive/stack-based traversal | O(V + E) |

## How to Use These Docs

1. **Learning**: Start with the intuition section to understand the core concept
2. **Interview Prep**: Focus on complexity analysis and trade-offs
3. **Implementation**: Use the step-by-step breakdowns and pseudocode
4. **Deep Dive**: Follow the mathematical proofs and further reading sections

## Notation Reference

| Symbol | Meaning |
|--------|---------|
| O(·)   | Big-O (upper bound) |
| Ω(·)   | Big-Omega (lower bound) |
| Θ(·)   | Big-Theta (tight bound) |
| α(n)   | Inverse Ackermann function |
| n      | Input size |
| V      | Number of vertices |
| E      | Number of edges |
| k      | Specific parameter (context-dependent) |
