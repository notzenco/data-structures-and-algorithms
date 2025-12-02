# Data Structures and Algorithms

A collection of classic data structures and algorithms implemented in multiple programming languages, with comprehensive documentation explaining the theory, math, and practical applications behind each one.

## Purpose

This repository serves as:

- **A learning resource** - Understand how fundamental CS concepts work under the hood
- **A reference implementation** - Clean, well-documented code in multiple languages
- **Interview preparation** - Each topic includes complexity analysis and common problems
- **A comparison tool** - See how the same algorithm looks across different languages

## Documentation

The [`docs/`](docs/) folder contains language-agnostic explanations covering:

- Intuition and visual walkthroughs
- Mathematical analysis and proofs
- Time/space complexity breakdowns
- Use cases and trade-offs
- Common interview problems

## Implementation Progress

| Status | Meaning |
|--------|---------|
| ❌ | Not implemented |
| ⚠️ | In progress |
| ✅ | Complete |

### Data Structures

| Data Structure | C | C++ | C# | Go | PHP | Python | Rust | TypeScript |
|----------------|---|-----|----|----|-----|--------|------|------------|
| [Dynamic Array](docs/data_structures/dynamic-array.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Singly Linked List](docs/data_structures/singly-linked-list.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Doubly Linked List](docs/data_structures/doubly-linked-list.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Stack](docs/data_structures/stack.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Queue](docs/data_structures/queue.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Deque](docs/data_structures/deque.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Hash Table](docs/data_structures/hash-table.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Binary Search Tree](docs/data_structures/binary-search-tree.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Binary Min Heap](docs/data_structures/heap.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Disjoint Set](docs/data_structures/disjoint-set.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |

### Algorithms

| Algorithm | C | C++ | C# | Go | PHP | Python | Rust | TypeScript |
|-----------|---|-----|----|----|-----|--------|------|------------|
| [Binary Search](docs/algorithms/searching/binary-search.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Insertion Sort](docs/algorithms/sorting/insertion-sort.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Merge Sort](docs/algorithms/sorting/merge-sort.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [Quick Sort](docs/algorithms/sorting/quick-sort.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [BFS](docs/algorithms/graph/bfs.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |
| [DFS](docs/algorithms/graph/dfs.md) | ✅ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ | ❌ |

## Project Structure

```
.
├── docs/                 # Theory, explanations, and analysis
│   ├── data_structures/  # Data structure documentation
│   └── algorithms/       # Algorithm documentation
└── <language>/           # Implementation folders (added as completed)
```

## License

Released under the MIT license. See [LICENSE](LICENSE) for details.
