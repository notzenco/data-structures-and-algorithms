# TypeScript Data Structures and Algorithms

This directory contains TypeScript implementations of common data structures and algorithms.

## Project Structure

```
typescript/
├── package.json              # NPM configuration
├── tsconfig.json             # TypeScript configuration
├── jest.config.js            # Jest test configuration
├── src/
│   ├── index.ts              # Main exports
│   ├── data-structures/      # Data structure implementations
│   │   ├── stack.ts
│   │   ├── queue.ts
│   │   ├── dynamic-array.ts
│   │   ├── singly-linked-list.ts
│   │   ├── doubly-linked-list.ts
│   │   ├── deque.ts
│   │   ├── hash-table.ts
│   │   ├── binary-search-tree.ts
│   │   ├── min-heap.ts
│   │   └── disjoint-set.ts
│   └── algorithms/           # Algorithm implementations
│       ├── binary-search.ts
│       ├── insertion-sort.ts
│       ├── merge-sort.ts
│       ├── quick-sort.ts
│       ├── graph.ts
│       ├── bfs.ts
│       └── dfs.ts
└── tests/
    ├── data-structures.test.ts
    └── algorithms.test.ts
```

## Requirements

- Node.js 18 or higher
- npm or yarn

## Installation

```bash
npm install
```

## Building

```bash
npm run build
```

## Running Tests

```bash
npm test
```

## Data Structures

| Structure | Description | Operations |
|-----------|-------------|------------|
| Stack | LIFO stack using linked nodes | push, pop, peek, isEmpty, size |
| Queue | FIFO queue using linked nodes | enqueue, dequeue, peek, isEmpty, size |
| DynamicArray | Growable array with amortized O(1) append | push, pop, get, set, remove, insert |
| SinglyLinkedList | Single-direction linked list | pushFront, pushBack, popFront, contains, remove |
| DoublyLinkedList | Bidirectional linked list | pushFront, pushBack, popFront, popBack, remove |
| Deque | Double-ended queue | pushFront, pushBack, popFront, popBack, peek |
| HashTable | Open addressing hash table with linear probing | put, get, remove, has |
| BinarySearchTree | BST with traversal methods | insert, remove, contains, min, max, inorder |
| MinHeap | Binary min-heap using array | insert, extractMin, peek |
| DisjointSet | Union-Find with path compression and union by rank | find, union, connected, count |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| Binary Search | Search in sorted array with bounds | O(log n) |
| Insertion Sort | Stable in-place sort | O(n²) average, O(n) best |
| Merge Sort | Stable divide-and-conquer sort | O(n log n) |
| Quick Sort | In-place partition sort with median-of-three | O(n log n) average |
| BFS | Breadth-first graph traversal | O(V + E) |
| DFS | Depth-first graph traversal | O(V + E) |

## Usage Examples

### Stack

```typescript
import { Stack } from 'dsa-typescript';

const stack = new Stack<number>();
stack.push(1);
stack.push(2);
stack.push(3);

console.log(stack.pop());  // 3
console.log(stack.peek()); // 2
```

### HashTable

```typescript
import { HashTable } from 'dsa-typescript';

const table = new HashTable<string, number>();
table.put('one', 1);
table.put('two', 2);

console.log(table.get('one')); // 1
console.log(table.has('three')); // false
```

### Sorting

```typescript
import { quickSort, mergeSort } from 'dsa-typescript';

const arr = [5, 2, 8, 1, 9];
quickSort(arr);
console.log(arr); // [1, 2, 5, 8, 9]

// With custom comparator
const words = ['banana', 'apple', 'cherry'];
mergeSort(words, (a, b) => a.localeCompare(b));
console.log(words); // ['apple', 'banana', 'cherry']
```

### Graph Algorithms

```typescript
import { Graph, bfs, bfsFindPath, bfsDistances, dfs, hasCycle } from 'dsa-typescript';

const graph = new Graph<string>();
graph.addEdge('A', 'B');
graph.addEdge('A', 'C');
graph.addEdge('B', 'D');
graph.addEdge('C', 'D');

// BFS traversal
const bfsResult = bfs(graph, 'A');
console.log(bfsResult); // ['A', 'B', 'C', 'D']

// Find shortest path
const path = bfsFindPath(graph, 'A', 'D');
console.log(path); // ['A', 'B', 'D'] or ['A', 'C', 'D']

// Get distances from source
const distances = bfsDistances(graph, 'A');
console.log(distances); // Map { 'A' => 0, 'B' => 1, 'C' => 1, 'D' => 2 }

// DFS with cycle detection (for directed graphs)
const directed = new Graph<string>(true);
directed.addEdge('A', 'B');
directed.addEdge('B', 'C');
directed.addEdge('C', 'A');
console.log(hasCycle(directed, 'A')); // true
```

## Design Notes

- Full TypeScript with strict type checking
- Generic implementations for type safety
- Comparator support for custom ordering
- Functional algorithms that work on arrays
- Graph supports both directed and undirected modes
