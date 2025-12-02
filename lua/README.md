# Data Structures and Algorithms - Lua

A comprehensive collection of data structures and algorithms implemented in Lua.

## Requirements

- Lua 5.3+

## Project Structure

```
lua/
├── init.lua
├── data_structures/
│   ├── stack.lua
│   ├── queue.lua
│   ├── dynamic_array.lua
│   ├── singly_linked_list.lua
│   ├── doubly_linked_list.lua
│   ├── deque.lua
│   ├── hash_table.lua
│   ├── binary_search_tree.lua
│   ├── min_heap.lua
│   └── disjoint_set.lua
├── algorithms/
│   ├── binary_search.lua
│   ├── insertion_sort.lua
│   ├── merge_sort.lua
│   ├── quick_sort.lua
│   ├── graph.lua
│   ├── bfs.lua
│   └── dfs.lua
├── tests/
│   └── test_all.lua
└── README.md
```

## Running Tests

```bash
cd lua/tests
lua test_all.lua
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
| BinarySearchTree | Ordered binary tree | insert, contains, findMin, findMax, inorder |
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

```lua
local Stack = require("data_structures.stack")

local s = Stack.new()
s:push(1)
s:push(2)
s:push(3)

print(s:pop())   -- 3
print(s:peek())  -- 2
print(s:size())  -- 2
```

### Queue

```lua
local Queue = require("data_structures.queue")

local q = Queue.new()
q:enqueue("first")
q:enqueue("second")

print(q:dequeue())  -- "first"
print(q:peek())     -- "second"
```

### Hash Table

```lua
local HashTable = require("data_structures.hash_table")

local ht = HashTable.new()
ht:put("one", 1)
ht:put("two", 2)

print(ht:get("one"))       -- 1
print(ht:contains("two"))  -- true
```

### Binary Search Tree

```lua
local BinarySearchTree = require("data_structures.binary_search_tree")

local tree = BinarySearchTree.new()
tree:insert(5)
tree:insert(3)
tree:insert(7)

print(tree:contains(5))  -- true
print(tree:findMin())    -- 3
-- tree:inorder() returns {3, 5, 7}
```

### Sorting

```lua
local quick_sort = require("algorithms.quick_sort")
local merge_sort = require("algorithms.merge_sort")

local arr = {5, 2, 8, 1, 9, 3}
quick_sort.sort(arr)
-- arr is now {1, 2, 3, 5, 8, 9}

local arr2 = {5, 2, 8, 1, 9, 3}
merge_sort.sort(arr2)
-- arr2 is now {1, 2, 3, 5, 8, 9}
```

### Graph Traversal

```lua
local Graph = require("algorithms.graph")
local bfs = require("algorithms.bfs")

local g = Graph.new(false)  -- undirected
g:addEdge(1, 2)
g:addEdge(1, 3)
g:addEdge(2, 4)

local order = bfs.traverse(g, 1)
-- order is {1, 2, 3, 4}

local path = bfs.shortestPath(g, 1, 4)
-- path is {1, 2, 4}
```

## Lua Features

This implementation leverages several Lua features:

- **Metatables**: Object-oriented programming with __index
- **Tables**: Used for both arrays and hash maps
- **Closures**: For encapsulation and private state
- **First-class functions**: For callbacks and comparators
- **Module system**: require/return pattern for modular code

## Complexity Reference

| Operation | Stack | Queue | DynamicArray | LinkedList | HashTable | BST | Heap | DisjointSet |
|-----------|-------|-------|--------------|------------|-----------|-----|------|-------------|
| Access | O(n) | O(n) | O(1) | O(n) | O(1)* | O(log n)* | O(1) | - |
| Search | O(n) | O(n) | O(n) | O(n) | O(1)* | O(log n)* | O(n) | - |
| Insert | O(1) | O(1)* | O(1)* | O(1) | O(1)* | O(log n)* | O(log n) | O(α(n)) |
| Delete | O(1) | O(1)* | O(n) | O(1) | O(1)* | O(log n)* | O(log n) | - |

\* Amortized / average case
