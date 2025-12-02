# Data Structures and Algorithms - Nim

A comprehensive collection of data structures and algorithms implemented in Nim.

## Requirements

- Nim 2.0.0+

## Project Structure

```
nim/
├── dsa.nimble
├── src/
│   ├── dsa.nim
│   └── dsa/
│       ├── data_structures/
│       │   ├── stack.nim
│       │   ├── queue.nim
│       │   ├── dynamic_array.nim
│       │   ├── singly_linked_list.nim
│       │   ├── doubly_linked_list.nim
│       │   ├── deque.nim
│       │   ├── hash_table.nim
│       │   ├── binary_search_tree.nim
│       │   ├── min_heap.nim
│       │   └── disjoint_set.nim
│       └── algorithms/
│           ├── binary_search.nim
│           ├── insertion_sort.nim
│           ├── merge_sort.nim
│           ├── quick_sort.nim
│           ├── graph.nim
│           ├── bfs.nim
│           └── dfs.nim
├── tests/
│   └── test_all.nim
└── README.md
```

## Building and Testing

```bash
# Install dependencies
nimble install

# Run all tests
nimble test

# Run individual module tests
nim c -r src/dsa/data_structures/stack.nim
```

## Data Structures

| Data Structure | Description | Operations |
|----------------|-------------|------------|
| Stack | LIFO container | push, pop, peek, isEmpty, size |
| Queue | FIFO container | enqueue, dequeue, peek, isEmpty, size |
| DynamicArray | Resizable array | push, pop, get, set, insert, removeAt |
| SinglyLinkedList | Forward-only linked list | prepend, append, get, removeAt |
| DoublyLinkedList | Bidirectional linked list | prepend, append, removeFirst, removeLast |
| Deque | Double-ended queue | pushFront, pushBack, popFront, popBack |
| HashTable | Open addressing hash map | put, get, remove, contains |
| BinarySearchTree | Ordered binary tree | insert, contains, findMin, findMax, remove |
| MinHeap | Binary min heap | insert, extractMin, peek, heapify |
| DisjointSet | Union-Find | makeSet, find, union, connected |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| BinarySearch | Search in sorted array | O(log n) |
| InsertionSort | Simple stable sort | O(n²) |
| MergeSort | Divide-and-conquer stable sort | O(n log n) |
| QuickSort | Median-of-three pivot sort | O(n log n) avg |
| Graph | Adjacency list representation | - |
| BFS | Breadth-first search | O(V + E) |
| DFS | Depth-first search | O(V + E) |

## Usage Examples

### Stack

```nim
import dsa

var s = newStack[int]()
s.push(1)
s.push(2)
s.push(3)

echo s.pop()   # 3
echo s.peek()  # 2
echo s.size()  # 2
```

### Queue

```nim
import dsa

var q = newQueue[string]()
q.enqueue("first")
q.enqueue("second")

echo q.dequeue()  # "first"
echo q.peek()     # "second"
```

### Hash Table

```nim
import dsa

var ht = newHashTable[string, int]()
ht["one"] = 1
ht["two"] = 2

echo ht["one"]           # 1
echo ht.contains("two")  # true
```

### Binary Search Tree

```nim
import dsa

var tree = newBinarySearchTree[int]()
tree.insert(5)
tree.insert(3)
tree.insert(7)

echo tree.contains(5)  # true
echo tree.findMin()    # 3
echo tree.inorder()    # @[3, 5, 7]
```

### Sorting

```nim
import dsa

var arr = @[5, 2, 8, 1, 9, 3]
quickSort(arr)
echo arr  # @[1, 2, 3, 5, 8, 9]

var arr2 = @[5, 2, 8, 1, 9, 3]
mergeSort(arr2)
echo arr2  # @[1, 2, 3, 5, 8, 9]
```

### Graph Traversal

```nim
import dsa

var g = newGraph[int](directed = false)
g.addEdge(1, 2)
g.addEdge(1, 3)
g.addEdge(2, 4)

let bfsOrder = bfs.traverse(g, 1)
echo bfsOrder  # @[1, 2, 3, 4]

let path = bfs.shortestPath(g, 1, 4)
echo path  # @[1, 2, 4]
```

## Nim Features

This implementation leverages several Nim features:

- **Generics**: Type-safe generic data structures using `[T]`
- **Ref types**: Reference semantics for linked structures
- **Operator overloading**: `[]` and `[]=` for natural syntax
- **Result type**: Implicit `result` variable
- **Iterators**: Custom iterators for traversal
- **Method chaining**: Fluent API where appropriate

## Complexity Reference

| Operation | Stack | Queue | DynamicArray | LinkedList | HashTable | BST | Heap | DisjointSet |
|-----------|-------|-------|--------------|------------|-----------|-----|------|-------------|
| Access | O(n) | O(n) | O(1) | O(n) | O(1)* | O(log n)* | O(1) | - |
| Search | O(n) | O(n) | O(n) | O(n) | O(1)* | O(log n)* | O(n) | - |
| Insert | O(1)* | O(1)* | O(1)* | O(1) | O(1)* | O(log n)* | O(log n) | O(α(n)) |
| Delete | O(1) | O(1)* | O(n) | O(1) | O(1)* | O(log n)* | O(log n) | - |

\* Amortized / average case
