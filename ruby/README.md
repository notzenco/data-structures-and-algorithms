# Ruby Data Structures and Algorithms

A comprehensive collection of data structures and algorithms implemented in Ruby.

## Requirements

- Ruby 3.0 or higher
- Bundler

## Installation

```bash
bundle install
```

## Running Tests

```bash
bundle exec rake test
```

Or directly with minitest:

```bash
ruby -Ilib:test test/data_structures_test.rb
ruby -Ilib:test test/algorithms_test.rb
```

## Project Structure

```
ruby/
├── Gemfile
├── Rakefile
├── lib/
│   ├── data_structures/
│   │   ├── stack.rb
│   │   ├── queue.rb
│   │   ├── dynamic_array.rb
│   │   ├── singly_linked_list.rb
│   │   ├── doubly_linked_list.rb
│   │   ├── deque.rb
│   │   ├── hash_table.rb
│   │   ├── binary_search_tree.rb
│   │   ├── min_heap.rb
│   │   └── disjoint_set.rb
│   └── algorithms/
│       ├── binary_search.rb
│       ├── insertion_sort.rb
│       ├── merge_sort.rb
│       ├── quick_sort.rb
│       ├── graph.rb
│       ├── bfs.rb
│       └── dfs.rb
└── test/
    ├── data_structures_test.rb
    └── algorithms_test.rb
```

## Data Structures

### Stack
LIFO (Last In, First Out) data structure.
- `push(value)` - Add element to top - O(1)
- `pop` - Remove and return top element - O(1)
- `peek` - View top element - O(1)
- `empty?` - Check if empty - O(1)
- `size` - Get number of elements - O(1)

### Queue
FIFO (First In, First Out) data structure using a linked list.
- `enqueue(value)` - Add element to back - O(1)
- `dequeue` - Remove and return front element - O(1)
- `peek` - View front element - O(1)
- `empty?` - Check if empty - O(1)
- `size` - Get number of elements - O(1)

### DynamicArray
Resizable array with automatic capacity management.
- `push(value)` - Add element to end - O(1) amortized
- `pop` - Remove and return last element - O(1)
- `get(index)` - Get element at index - O(1)
- `set(index, value)` - Set element at index - O(1)
- `insert(index, value)` - Insert at index - O(n)
- `remove_at(index)` - Remove at index - O(n)

### SinglyLinkedList
Linear collection with forward traversal.
- `append(value)` - Add to end - O(n)
- `prepend(value)` - Add to front - O(1)
- `insert(index, value)` - Insert at index - O(n)
- `remove_at(index)` - Remove at index - O(n)
- `get(index)` - Get element at index - O(n)
- `contains?(value)` - Check if value exists - O(n)

### DoublyLinkedList
Linear collection with bidirectional traversal.
- `append(value)` - Add to end - O(1)
- `prepend(value)` - Add to front - O(1)
- `remove_first` - Remove from front - O(1)
- `remove_last` - Remove from end - O(1)
- `first` / `last` - Access ends - O(1)

### Deque
Double-ended queue supporting operations at both ends.
- `push_front(value)` / `push_back(value)` - Add to either end - O(1)
- `pop_front` / `pop_back` - Remove from either end - O(1)
- `peek_front` / `peek_back` - View either end - O(1)

### HashTable
Key-value store with open addressing and linear probing.
- `put(key, value)` - Insert or update - O(1) average
- `get(key)` - Retrieve value - O(1) average
- `remove(key)` - Delete entry - O(1) average
- `contains?(key)` - Check key exists - O(1) average
- `keys` / `values` - Get all keys/values - O(n)

### BinarySearchTree
Ordered tree structure for efficient searching.
- `insert(value)` - Add element - O(log n) average
- `contains?(value)` - Check if exists - O(log n) average
- `remove(value)` - Delete element - O(log n) average
- `min` / `max` - Get extremes - O(log n)
- `in_order` - Get sorted elements - O(n)
- Supports custom comparators via block

### MinHeap
Priority queue with smallest element at top.
- `insert(value)` - Add element - O(log n)
- `extract_min` - Remove smallest - O(log n)
- `peek` - View smallest - O(1)
- Supports custom comparators via block

### DisjointSet (Union-Find)
Tracks elements partitioned into disjoint sets.
- `find(x)` - Find set representative - O(α(n)) amortized
- `union(x, y)` - Merge sets - O(α(n)) amortized
- `connected?(x, y)` - Check same set - O(α(n)) amortized
- Uses path compression and union by rank

## Algorithms

### Binary Search
Search in sorted arrays with O(log n) complexity.
- `search(arr, target)` - Find element index
- `lower_bound(arr, target)` - First index >= target
- `upper_bound(arr, target)` - First index > target

### Sorting Algorithms

#### Insertion Sort
- Time: O(n²) worst, O(n) best (nearly sorted)
- Space: O(1)
- Stable: Yes
- `sort!(arr)` - In-place sort
- `sort(arr)` - Return sorted copy

#### Merge Sort
- Time: O(n log n) all cases
- Space: O(n)
- Stable: Yes
- `sort!(arr)` - In-place sort
- `sort(arr)` - Return sorted copy

#### Quick Sort
- Time: O(n log n) average, O(n²) worst
- Space: O(log n)
- Stable: No
- Uses median-of-three pivot selection
- `sort!(arr)` - In-place sort
- `sort(arr)` - Return sorted copy

### Graph Algorithms

#### Graph
Adjacency list representation supporting directed and undirected graphs.
- `add_vertex(v)` - Add vertex
- `add_edge(from, to)` - Add edge
- `remove_edge(from, to)` - Remove edge
- `neighbors(v)` - Get adjacent vertices

#### BFS (Breadth-First Search)
- `traverse(graph, start)` - Level-order traversal
- `shortest_path(graph, start, finish)` - Unweighted shortest path
- `distances(graph, start)` - Distances to all reachable vertices

#### DFS (Depth-First Search)
- `traverse(graph, start)` - Iterative DFS
- `traverse_recursive(graph, start)` - Recursive DFS
- `find_path(graph, start, finish)` - Find any path
- `has_cycle?(graph)` - Cycle detection
- `topological_sort(graph)` - DAG ordering

## Usage Examples

```ruby
require_relative 'lib/data_structures/stack'
require_relative 'lib/data_structures/min_heap'
require_relative 'lib/algorithms/quick_sort'
require_relative 'lib/algorithms/graph'
require_relative 'lib/algorithms/bfs'

# Stack
stack = DSA::DataStructures::Stack.new
stack.push(1).push(2)
puts stack.pop # 2

# MinHeap with custom comparator (max heap)
max_heap = DSA::DataStructures::MinHeap.new { |a, b| b <=> a }
max_heap.insert(5).insert(10)
puts max_heap.extract_min # 10

# Sorting
arr = [64, 34, 25, 12, 22, 11, 90]
DSA::Algorithms::QuickSort.sort!(arr)
# arr is now [11, 12, 22, 25, 34, 64, 90]

# Graph with BFS
graph = DSA::Algorithms::Graph.new
graph.add_edge(1, 2).add_edge(1, 3).add_edge(2, 4).add_edge(3, 4)

path = DSA::Algorithms::BFS.shortest_path(graph, 1, 4)
# path is [1, 2, 4] or [1, 3, 4]
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
