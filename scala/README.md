# Data Structures and Algorithms - Scala

A comprehensive collection of data structures and algorithms implemented in Scala 3.

## Requirements

- Scala 3.3.1+
- sbt 1.9+

## Project Structure

```
scala/
├── build.sbt
├── src/
│   ├── main/scala/
│   │   ├── datastructures/
│   │   │   ├── Stack.scala
│   │   │   ├── Queue.scala
│   │   │   ├── DynamicArray.scala
│   │   │   ├── SinglyLinkedList.scala
│   │   │   ├── DoublyLinkedList.scala
│   │   │   ├── Deque.scala
│   │   │   ├── HashTable.scala
│   │   │   ├── BinarySearchTree.scala
│   │   │   ├── MinHeap.scala
│   │   │   └── DisjointSet.scala
│   │   └── algorithms/
│   │       ├── BinarySearch.scala
│   │       ├── InsertionSort.scala
│   │       ├── MergeSort.scala
│   │       ├── QuickSort.scala
│   │       ├── Graph.scala
│   │       ├── BFS.scala
│   │       └── DFS.scala
│   └── test/scala/
│       ├── DataStructuresTest.scala
│       └── AlgorithmsTest.scala
└── README.md
```

## Building and Testing

```bash
# Compile
sbt compile

# Run tests
sbt test

# Run a specific test class
sbt "testOnly DataStructuresTest"
sbt "testOnly AlgorithmsTest"

# Generate console REPL
sbt console
```

## Data Structures

| Data Structure | Description | Operations |
|----------------|-------------|------------|
| Stack | LIFO container | push, pop, peek, isEmpty, size |
| Queue | FIFO container | enqueue, dequeue, peek, isEmpty, size |
| DynamicArray | Resizable array | push, pop, get, set, insert, removeAt |
| SinglyLinkedList | Forward-only linked list | prepend, append, insert, removeAt, get |
| DoublyLinkedList | Bidirectional linked list | prepend, append, removeFirst, removeLast |
| Deque | Double-ended queue | pushFront, pushBack, popFront, popBack |
| HashTable | Open addressing hash map | put, get, remove, contains, keys, values |
| BinarySearchTree | Ordered binary tree | insert, remove, contains, min, max, traversals |
| MinHeap | Priority queue | insert, extractMin, peek |
| DisjointSet | Union-Find structure | makeSet, find, union, connected |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| BinarySearch | Search in sorted array | O(log n) |
| InsertionSort | Simple stable sort | O(n²) |
| MergeSort | Divide-and-conquer sort | O(n log n) |
| QuickSort | Median-of-three pivot | O(n log n) avg |
| Graph | Adjacency list representation | - |
| BFS | Breadth-first search | O(V + E) |
| DFS | Depth-first search | O(V + E) |

## Usage Examples

### Stack

```scala
import datastructures.Stack

val stack = Stack[Int]()
stack.push(1).push(2).push(3)
println(stack.pop())   // Some(3)
println(stack.peek)    // Some(2)
```

### BinarySearchTree

```scala
import datastructures.BinarySearchTree

val bst = BinarySearchTree[Int]()
bst.insert(5).insert(3).insert(7).insert(1).insert(9)
println(bst.inorder)   // List(1, 3, 5, 7, 9)
println(bst.min)       // Some(1)
println(bst.contains(5)) // true
```

### MinHeap

```scala
import datastructures.MinHeap

val heap = MinHeap.from(List(5, 3, 7, 1, 9))
println(heap.extractMin()) // Some(1)
println(heap.extractMin()) // Some(3)
```

### Sorting

```scala
import algorithms.{QuickSort, MergeSort}

val arr = Array(5, 2, 8, 1, 9, 3)
QuickSort.sort(arr)
println(arr.mkString(", ")) // 1, 2, 3, 5, 8, 9

// Immutable version
val arr2 = Array(5, 2, 8, 1, 9, 3)
val sorted = MergeSort.sorted(arr2) // Original unchanged
```

### Graph Traversals

```scala
import algorithms.{Graph, BFS, DFS}

val graph = Graph[Int]()
graph.addEdge(1, 2).addEdge(1, 3).addEdge(2, 4).addEdge(3, 4)

println(BFS.traverse(graph, 1))          // List(1, 2, 3, 4)
println(BFS.shortestPath(graph, 1, 4))   // Some(List(1, 2, 4)) or similar
println(BFS.distances(graph, 1))         // Map(1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2)

println(DFS.traverse(graph, 1))          // List(1, 3, 4, 2) or similar
```

### DisjointSet

```scala
import datastructures.DisjointSet

val ds = DisjointSet[Int]()
ds.makeSet(1).makeSet(2).makeSet(3).makeSet(4)

ds.union(1, 2)
ds.union(3, 4)
println(ds.connected(1, 2)) // true
println(ds.connected(1, 3)) // false
println(ds.setCount)        // 2

ds.union(1, 3)
println(ds.connected(1, 4)) // true
println(ds.setCount)        // 1
```

## Scala 3 Features

This implementation leverages several Scala 3 features:

- **New syntax**: Indentation-based syntax, `then` in conditionals
- **Context parameters**: `using` for Ordering instances
- **Extension methods**: Optional extensions for common operations
- **Enums**: Could be used for graph types
- **Opaque types**: Could be used for type safety

## Complexity Reference

### Data Structures

| Operation | Stack | Queue | DynamicArray | LinkedList | HashTable | BST | Heap | DisjointSet |
|-----------|-------|-------|--------------|------------|-----------|-----|------|-------------|
| Access | O(n) | O(n) | O(1) | O(n) | O(1)* | O(log n)* | O(1) | - |
| Search | O(n) | O(n) | O(n) | O(n) | O(1)* | O(log n)* | O(n) | - |
| Insert | O(1) | O(1) | O(1)* | O(1)** | O(1)* | O(log n)* | O(log n) | O(α(n)) |
| Delete | O(1) | O(1) | O(n) | O(1)** | O(1)* | O(log n)* | O(log n) | - |

\* Average case, worst case may differ
\*\* At head/tail; O(n) for arbitrary position

### Sorting Algorithms

| Algorithm | Best | Average | Worst | Space | Stable |
|-----------|------|---------|-------|-------|--------|
| InsertionSort | O(n) | O(n²) | O(n²) | O(1) | Yes |
| MergeSort | O(n log n) | O(n log n) | O(n log n) | O(n) | Yes |
| QuickSort | O(n log n) | O(n log n) | O(n²) | O(log n) | No |
