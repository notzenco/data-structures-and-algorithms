# Data Structures and Algorithms - Zig

A comprehensive collection of data structures and algorithms implemented in Zig.

## Requirements

- Zig 0.11.0+

## Project Structure

```
zig/
├── build.zig
├── src/
│   ├── lib.zig
│   ├── data_structures/
│   │   ├── stack.zig
│   │   ├── queue.zig
│   │   ├── dynamic_array.zig
│   │   ├── singly_linked_list.zig
│   │   ├── doubly_linked_list.zig
│   │   ├── deque.zig
│   │   ├── hash_table.zig
│   │   ├── binary_search_tree.zig
│   │   ├── min_heap.zig
│   │   └── disjoint_set.zig
│   └── algorithms/
│       ├── binary_search.zig
│       ├── insertion_sort.zig
│       ├── merge_sort.zig
│       ├── quick_sort.zig
│       ├── graph.zig
│       ├── bfs.zig
│       └── dfs.zig
└── README.md
```

## Building and Testing

```bash
# Build the library
zig build

# Run all tests
zig build test

# Run tests for a specific file
zig test src/data_structures/stack.zig
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
| HashTable | AutoHashMap wrapper | put, get, remove, contains |
| BinarySearchTree | Ordered binary tree | insert, contains, findMin, findMax |
| MinHeap | Priority queue | insert, extractMin, peek |
| DisjointSet | Union-Find | makeSet, find, unionSets, connected |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| BinarySearch | Search in sorted slice | O(log n) |
| InsertionSort | Simple stable sort | O(n²) |
| MergeSort | Divide-and-conquer sort | O(n log n) |
| QuickSort | Median-of-three pivot | O(n log n) avg |
| Graph | Adjacency list representation | - |
| BFS | Breadth-first search | O(V + E) |
| DFS | Depth-first search | O(V + E) |

## Usage Examples

### Stack

```zig
const std = @import("std");
const Stack = @import("dsa").data_structures.Stack;

pub fn main() !void {
    var stack = Stack(i32).init(std.heap.page_allocator);
    defer stack.deinit();

    try stack.push(1);
    try stack.push(2);
    try stack.push(3);

    _ = stack.pop(); // 3
    _ = stack.peek(); // 2
}
```

### Sorting

```zig
const quick_sort = @import("dsa").algorithms.quick_sort;

var arr = [_]i32{ 5, 2, 8, 1, 9, 3 };
quick_sort.sort(i32, &arr);
// arr is now { 1, 2, 3, 5, 8, 9 }
```

## Zig Features

This implementation leverages several Zig features:

- **Comptime generics**: Type-safe generic data structures
- **Explicit allocators**: All allocations require an allocator parameter
- **Error handling**: Using `!void` and error unions
- **Optional types**: Using `?T` for nullable values
- **Testing**: Built-in test blocks with `std.testing`

## Memory Management

All data structures require an allocator and must be properly deinitialized:

```zig
var ds = DataStructure.init(allocator);
defer ds.deinit();
```

## Complexity Reference

| Operation | Stack | Queue | DynamicArray | LinkedList | HashTable | BST | Heap | DisjointSet |
|-----------|-------|-------|--------------|------------|-----------|-----|------|-------------|
| Access | O(n) | O(n) | O(1) | O(n) | O(1)* | O(log n)* | O(1) | - |
| Search | O(n) | O(n) | O(n) | O(n) | O(1)* | O(log n)* | O(n) | - |
| Insert | O(1)* | O(1)* | O(1)* | O(1) | O(1)* | O(log n)* | O(log n) | O(α(n)) |
| Delete | O(1) | O(1)* | O(n) | O(1) | O(1)* | O(log n)* | O(log n) | - |

\* Amortized / average case
