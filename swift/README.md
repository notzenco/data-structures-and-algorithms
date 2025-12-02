# Swift Data Structures and Algorithms

A comprehensive collection of data structures and algorithms implemented in Swift.

## Requirements

- Swift 5.9 or higher
- Xcode 15+ (for macOS/iOS development)

## Building

```bash
swift build
```

## Running Tests

```bash
swift test
```

## Project Structure

```
swift/
├── Package.swift
├── Sources/
│   └── DSA/
│       ├── DataStructures/
│       │   ├── Stack.swift
│       │   ├── Queue.swift
│       │   ├── DynamicArray.swift
│       │   ├── SinglyLinkedList.swift
│       │   ├── DoublyLinkedList.swift
│       │   ├── Deque.swift
│       │   ├── HashTable.swift
│       │   ├── BinarySearchTree.swift
│       │   ├── MinHeap.swift
│       │   └── DisjointSet.swift
│       └── Algorithms/
│           ├── BinarySearch.swift
│           ├── InsertionSort.swift
│           ├── MergeSort.swift
│           ├── QuickSort.swift
│           ├── Graph.swift
│           ├── BFS.swift
│           └── DFS.swift
└── Tests/
    └── DSATests/
        ├── DataStructuresTests.swift
        └── AlgorithmsTests.swift
```

## Data Structures

### Stack
LIFO (Last In, First Out) data structure.
- `push(_:)` - Add element to top - O(1)
- `pop()` - Remove and return top element - O(1)
- `peek()` - View top element - O(1)
- `isEmpty` - Check if empty - O(1)
- `count` - Get number of elements - O(1)

### Queue
FIFO (First In, First Out) data structure using a linked list.
- `enqueue(_:)` - Add element to back - O(1)
- `dequeue()` - Remove and return front element - O(1)
- `peek()` - View front element - O(1)
- `isEmpty` - Check if empty - O(1)
- `count` - Get number of elements - O(1)

### DynamicArray
Resizable array with automatic capacity management.
- `push(_:)` - Add element to end - O(1) amortized
- `pop()` - Remove and return last element - O(1)
- `get(_:)` - Get element at index - O(1)
- `set(_:_:)` - Set element at index - O(1)
- `insert(_:_:)` - Insert at index - O(n)
- `removeAt(_:)` - Remove at index - O(n)

### SinglyLinkedList
Linear collection with forward traversal.
- `append(_:)` - Add to end - O(n)
- `prepend(_:)` - Add to front - O(1)
- `insert(_:_:)` - Insert at index - O(n)
- `removeAt(_:)` - Remove at index - O(n)
- `get(_:)` - Get element at index - O(n)
- `contains(_:)` - Check if value exists - O(n)

### DoublyLinkedList
Linear collection with bidirectional traversal.
- `append(_:)` - Add to end - O(1)
- `prepend(_:)` - Add to front - O(1)
- `removeFirst()` - Remove from front - O(1)
- `removeLast()` - Remove from end - O(1)
- `getFirst()` / `getLast()` - Access ends - O(1)

### Deque
Double-ended queue supporting operations at both ends.
- `pushFront(_:)` / `pushBack(_:)` - Add to either end - O(1)
- `popFront()` / `popBack()` - Remove from either end - O(1)
- `peekFront()` / `peekBack()` - View either end - O(1)

### HashTable
Key-value store with open addressing and linear probing.
- `put(_:_:)` - Insert or update - O(1) average
- `get(_:)` - Retrieve value - O(1) average
- `remove(_:)` - Delete entry - O(1) average
- `contains(_:)` - Check key exists - O(1) average
- `keys()` / `values()` - Get all keys/values - O(n)

### BinarySearchTree
Ordered tree structure for efficient searching.
- `insert(_:)` - Add element - O(log n) average
- `contains(_:)` - Check if exists - O(log n) average
- `remove(_:)` - Delete element - O(log n) average
- `min()` / `max()` - Get extremes - O(log n)
- `inOrder()` - Get sorted elements - O(n)
- Supports custom comparators

### MinHeap
Priority queue with smallest element at top.
- `insert(_:)` - Add element - O(log n)
- `extractMin()` - Remove smallest - O(log n)
- `peek()` - View smallest - O(1)
- Supports custom comparators

### DisjointSet (Union-Find)
Tracks elements partitioned into disjoint sets.
- `find(_:)` - Find set representative - O(α(n)) amortized
- `union(_:_:)` - Merge sets - O(α(n)) amortized
- `connected(_:_:)` - Check same set - O(α(n)) amortized
- Uses path compression and union by rank

## Algorithms

### Binary Search
Search in sorted arrays with O(log n) complexity.
- `search(_:_:)` - Find element index
- `lowerBound(_:_:)` - First index >= target
- `upperBound(_:_:)` - First index > target

### Sorting Algorithms

#### Insertion Sort
- Time: O(n²) worst, O(n) best (nearly sorted)
- Space: O(1)
- Stable: Yes
- `sort(_:by:)` - In-place sort
- `sorted(_:by:)` - Return sorted copy

#### Merge Sort
- Time: O(n log n) all cases
- Space: O(n)
- Stable: Yes
- `sort(_:by:)` - In-place sort
- `sorted(_:by:)` - Return sorted copy

#### Quick Sort
- Time: O(n log n) average, O(n²) worst
- Space: O(log n)
- Stable: No
- Uses median-of-three pivot selection
- `sort(_:by:)` - In-place sort
- `sorted(_:by:)` - Return sorted copy

### Graph Algorithms

#### Graph
Adjacency list representation supporting directed and undirected graphs.
- `addVertex(_:)` - Add vertex
- `addEdge(from:to:)` - Add edge
- `removeEdge(from:to:)` - Remove edge
- `getNeighbors(_:)` - Get adjacent vertices

#### BFS (Breadth-First Search)
- `traverse(_:from:)` - Level-order traversal
- `shortestPath(_:from:to:)` - Unweighted shortest path
- `distances(_:from:)` - Distances to all reachable vertices

#### DFS (Depth-First Search)
- `traverse(_:from:)` - Iterative DFS
- `traverseRecursive(_:from:)` - Recursive DFS
- `findPath(_:from:to:)` - Find any path
- `hasCycle(_:)` - Cycle detection
- `topologicalSort(_:)` - DAG ordering

## Usage Examples

```swift
import DSA

// Stack
var stack = Stack<Int>()
stack.push(1)
stack.push(2)
print(stack.pop()!) // 2

// MinHeap with custom comparator (max heap)
var maxHeap = MinHeap<Int> { $0 > $1 ? -1 : ($0 < $1 ? 1 : 0) }
maxHeap.insert(5)
maxHeap.insert(10)
print(maxHeap.extractMin()!) // 10

// Sorting
var arr = [64, 34, 25, 12, 22, 11, 90]
QuickSort.sort(&arr)
// arr is now [11, 12, 22, 25, 34, 64, 90]

// Graph with BFS
let graph = Graph<Int>()
graph.addEdge(from: 1, to: 2)
graph.addEdge(from: 1, to: 3)
graph.addEdge(from: 2, to: 4)
graph.addEdge(from: 3, to: 4)

let path = BFS.shortestPath(graph, from: 1, to: 4)
// path is [1, 2, 4] or [1, 3, 4]
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
