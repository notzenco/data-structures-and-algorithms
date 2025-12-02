# PHP Data Structures and Algorithms

A comprehensive collection of data structures and algorithms implemented in PHP 8.1+.

## Requirements

- PHP 8.1 or higher
- Composer

## Installation

```bash
composer install
```

## Running Tests

```bash
./vendor/bin/phpunit tests/
```

## Project Structure

```
php/
├── composer.json
├── src/
│   ├── DataStructures/
│   │   ├── Stack.php
│   │   ├── Queue.php
│   │   ├── DynamicArray.php
│   │   ├── SinglyLinkedList.php
│   │   ├── DoublyLinkedList.php
│   │   ├── Deque.php
│   │   ├── HashTable.php
│   │   ├── BinarySearchTree.php
│   │   ├── MinHeap.php
│   │   └── DisjointSet.php
│   └── Algorithms/
│       ├── BinarySearch.php
│       ├── InsertionSort.php
│       ├── MergeSort.php
│       ├── QuickSort.php
│       ├── Graph.php
│       ├── BFS.php
│       └── DFS.php
└── tests/
    ├── DataStructuresTest.php
    └── AlgorithmsTest.php
```

## Data Structures

### Stack
LIFO (Last In, First Out) data structure.
- `push(value)` - Add element to top - O(1)
- `pop()` - Remove and return top element - O(1)
- `peek()` - View top element - O(1)
- `isEmpty()` - Check if empty - O(1)
- `size()` - Get number of elements - O(1)

### Queue
FIFO (First In, First Out) data structure.
- `enqueue(value)` - Add element to back - O(1)
- `dequeue()` - Remove and return front element - O(1)
- `peek()` - View front element - O(1)
- `isEmpty()` - Check if empty - O(1)
- `size()` - Get number of elements - O(1)

### DynamicArray
Resizable array with automatic capacity management.
- `push(value)` - Add element to end - O(1) amortized
- `pop()` - Remove and return last element - O(1)
- `get(index)` - Get element at index - O(1)
- `set(index, value)` - Set element at index - O(1)
- `insert(index, value)` - Insert at index - O(n)
- `removeAt(index)` - Remove at index - O(n)

### SinglyLinkedList
Linear collection with forward traversal.
- `append(value)` - Add to end - O(n)
- `prepend(value)` - Add to front - O(1)
- `insert(index, value)` - Insert at index - O(n)
- `removeAt(index)` - Remove at index - O(n)
- `get(index)` - Get element at index - O(n)
- `contains(value)` - Check if value exists - O(n)

### DoublyLinkedList
Linear collection with bidirectional traversal.
- `append(value)` - Add to end - O(1)
- `prepend(value)` - Add to front - O(1)
- `removeFirst()` - Remove from front - O(1)
- `removeLast()` - Remove from end - O(1)
- `getFirst()` / `getLast()` - Access ends - O(1)

### Deque
Double-ended queue supporting operations at both ends.
- `pushFront(value)` / `pushBack(value)` - Add to either end - O(1)
- `popFront()` / `popBack()` - Remove from either end - O(1)
- `peekFront()` / `peekBack()` - View either end - O(1)

### HashTable
Key-value store with open addressing and linear probing.
- `put(key, value)` - Insert or update - O(1) average
- `get(key)` - Retrieve value - O(1) average
- `remove(key)` - Delete entry - O(1) average
- `contains(key)` - Check key exists - O(1) average
- `keys()` / `values()` - Get all keys/values - O(n)

### BinarySearchTree
Ordered tree structure for efficient searching.
- `insert(value)` - Add element - O(log n) average
- `contains(value)` - Check if exists - O(log n) average
- `remove(value)` - Delete element - O(log n) average
- `min()` / `max()` - Get extremes - O(log n)
- `inOrder()` - Get sorted elements - O(n)

### MinHeap
Priority queue with smallest element at top.
- `insert(value)` - Add element - O(log n)
- `extractMin()` - Remove smallest - O(log n)
- `peek()` - View smallest - O(1)
- Supports custom comparators

### DisjointSet (Union-Find)
Tracks elements partitioned into disjoint sets.
- `find(x)` - Find set representative - O(α(n)) amortized
- `union(x, y)` - Merge sets - O(α(n)) amortized
- `connected(x, y)` - Check same set - O(α(n)) amortized
- Uses path compression and union by rank

## Algorithms

### Binary Search
Search in sorted arrays with O(log n) complexity.
- `search(arr, target)` - Find element index
- `lowerBound(arr, target)` - First index >= target
- `upperBound(arr, target)` - First index > target

### Sorting Algorithms

#### Insertion Sort
- Time: O(n²) worst, O(n) best (nearly sorted)
- Space: O(1)
- Stable: Yes
- `sort(&arr)` - In-place sort
- `sorted(arr)` - Return sorted copy

#### Merge Sort
- Time: O(n log n) all cases
- Space: O(n)
- Stable: Yes
- `sort(&arr)` - In-place sort
- `sorted(arr)` - Return sorted copy

#### Quick Sort
- Time: O(n log n) average, O(n²) worst
- Space: O(log n)
- Stable: No
- Uses median-of-three pivot selection
- `sort(&arr)` - In-place sort
- `sorted(arr)` - Return sorted copy

### Graph Algorithms

#### Graph
Adjacency list representation supporting directed and undirected graphs.
- `addVertex(v)` - Add vertex
- `addEdge(from, to)` - Add edge
- `removeEdge(from, to)` - Remove edge
- `getNeighbors(v)` - Get adjacent vertices

#### BFS (Breadth-First Search)
- `traverse(graph, start)` - Level-order traversal
- `shortestPath(graph, start, end)` - Unweighted shortest path
- `distances(graph, start)` - Distances to all reachable vertices

#### DFS (Depth-First Search)
- `traverse(graph, start)` - Iterative DFS
- `traverseRecursive(graph, start)` - Recursive DFS
- `findPath(graph, start, end)` - Find any path
- `hasCycle(graph)` - Cycle detection
- `topologicalSort(graph)` - DAG ordering

## Usage Examples

```php
<?php

use DSA\DataStructures\Stack;
use DSA\DataStructures\MinHeap;
use DSA\Algorithms\QuickSort;
use DSA\Algorithms\Graph;
use DSA\Algorithms\BFS;

// Stack
$stack = new Stack();
$stack->push(1);
$stack->push(2);
echo $stack->pop(); // 2

// MinHeap with custom comparator (max heap)
$maxHeap = new MinHeap(fn($a, $b) => $b <=> $a);
$maxHeap->insert(5);
$maxHeap->insert(10);
echo $maxHeap->extractMin(); // 10

// Sorting
$arr = [64, 34, 25, 12, 22, 11, 90];
QuickSort::sort($arr);
// $arr is now [11, 12, 22, 25, 34, 64, 90]

// Graph with BFS
$graph = new Graph();
$graph->addEdge(1, 2);
$graph->addEdge(1, 3);
$graph->addEdge(2, 4);
$graph->addEdge(3, 4);

$path = BFS::shortestPath($graph, 1, 4);
// $path is [1, 2, 4] or [1, 3, 4]
```

## Complexity Summary

| Data Structure | Access | Search | Insert | Delete |
|---------------|--------|--------|--------|--------|
| DynamicArray | O(1) | O(n) | O(n)* | O(n) |
| SinglyLinkedList | O(n) | O(n) | O(1)** | O(n) |
| DoublyLinkedList | O(n) | O(n) | O(1) | O(1)*** |
| HashTable | - | O(1) | O(1) | O(1) |
| BinarySearchTree | - | O(log n) | O(log n) | O(log n) |
| MinHeap | - | O(n) | O(log n) | O(log n) |

\* O(1) amortized for push
\** O(1) for prepend, O(n) for arbitrary insert
\*** O(1) for ends, O(n) for arbitrary delete

## License

MIT
