# Data Structures and Algorithms - Elixir

A comprehensive collection of functional data structures and algorithms implemented in Elixir.

## Requirements

- Elixir 1.14+
- Erlang/OTP 25+

## Project Structure

```
elixir/
├── mix.exs
├── lib/
│   ├── data_structures/
│   │   ├── stack.ex
│   │   ├── queue.ex
│   │   ├── dynamic_array.ex
│   │   ├── singly_linked_list.ex
│   │   ├── doubly_linked_list.ex
│   │   ├── deque.ex
│   │   ├── hash_table.ex
│   │   ├── binary_search_tree.ex
│   │   ├── min_heap.ex
│   │   └── disjoint_set.ex
│   └── algorithms/
│       ├── binary_search.ex
│       ├── insertion_sort.ex
│       ├── merge_sort.ex
│       ├── quick_sort.ex
│       ├── graph.ex
│       ├── bfs.ex
│       └── dfs.ex
├── test/
│   ├── test_helper.exs
│   ├── data_structures_test.exs
│   └── algorithms_test.exs
└── README.md
```

## Building and Testing

```bash
# Install dependencies
mix deps.get

# Compile
mix compile

# Run tests
mix test

# Run specific test file
mix test test/data_structures_test.exs

# Start interactive shell
iex -S mix
```

## Data Structures

| Data Structure | Description | Operations |
|----------------|-------------|------------|
| Stack | LIFO container | push, pop, peek, empty?, size |
| Queue | FIFO container (banker's queue) | enqueue, dequeue, peek, empty?, size |
| DynamicArray | List-backed array | push, pop, get, set, insert_at, remove_at |
| SinglyLinkedList | Forward-only linked list | prepend, append, insert_at, remove_at, get |
| DoublyLinkedList | Bidirectional list | prepend, append, remove_first, remove_last |
| Deque | Double-ended queue | push_front, push_back, pop_front, pop_back |
| HashTable | Map wrapper | put, get, remove, contains?, keys, values |
| BinarySearchTree | Ordered binary tree | insert, remove, contains?, find_min, find_max |
| MinHeap | Priority queue | insert, extract_min, peek |
| DisjointSet | Union-Find | make_set, find, union, connected? |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| BinarySearch | Search in sorted list | O(log n) |
| InsertionSort | Simple stable sort | O(n²) |
| MergeSort | Divide-and-conquer sort | O(n log n) |
| QuickSort | Median-of-three pivot | O(n log n) avg |
| Graph | Adjacency list representation | - |
| BFS | Breadth-first search | O(V + E) |
| DFS | Depth-first search | O(V + E) |

## Usage Examples

### Stack

```elixir
alias DataStructures.Stack

stack = Stack.new()
        |> Stack.push(1)
        |> Stack.push(2)
        |> Stack.push(3)

{:ok, 3} = Stack.peek(stack)
{:ok, 3, stack} = Stack.pop(stack)
```

### BinarySearchTree

```elixir
alias DataStructures.BinarySearchTree, as: BST

bst = BST.from_list([5, 3, 7, 1, 9])
BST.inorder(bst)          # [1, 3, 5, 7, 9]
{:ok, 1} = BST.find_min(bst)
BST.contains?(bst, 5)     # true
```

### MinHeap

```elixir
alias DataStructures.MinHeap

heap = MinHeap.from_list([5, 3, 7, 1, 9])
MinHeap.to_list(heap)     # [1, 3, 5, 7, 9]
{:ok, 1} = MinHeap.peek(heap)
```

### Sorting

```elixir
alias Algorithms.{MergeSort, QuickSort}

list = [5, 2, 8, 1, 9, 3]
MergeSort.sort(list)      # [1, 2, 3, 5, 8, 9]
QuickSort.sort(list)      # [1, 2, 3, 5, 8, 9]
```

### Graph Traversals

```elixir
alias Algorithms.{Graph, BFS, DFS}

graph = Graph.undirected()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(1, 3)
        |> Graph.add_edge(2, 4)
        |> Graph.add_edge(3, 4)

BFS.traverse(graph, 1)           # [1, 2, 3, 4]
BFS.shortest_path(graph, 1, 4)   # {:ok, [1, 2, 4]} or similar
BFS.distances(graph, 1)          # %{1 => 0, 2 => 1, 3 => 1, 4 => 2}

DFS.traverse(graph, 1)           # [1, ...]
DFS.has_cycle?(graph)            # depends on graph
```

### DisjointSet

```elixir
alias DataStructures.DisjointSet

ds = DisjointSet.new()
     |> DisjointSet.make_set(1)
     |> DisjointSet.make_set(2)
     |> DisjointSet.make_set(3)
     |> DisjointSet.make_set(4)

DisjointSet.connected?(ds, 1, 2)  # false

{:ok, ds} = DisjointSet.union(ds, 1, 2)
DisjointSet.connected?(ds, 1, 2)  # true
DisjointSet.set_count(ds)         # 3
```

## Functional Programming Notes

All implementations are purely functional and immutable:

- **Immutability**: All data structures return new instances on modification
- **Pattern matching**: Extensively used for clear, readable code
- **Structs**: Each data structure is a proper Elixir struct with typespecs
- **Error handling**: Operations return tagged tuples `{:ok, value}` or `{:error, reason}`

### Elixir Idioms

- **Pipe operator**: All operations are designed for `|>` chaining
- **Structs with @type**: Full typespec coverage
- **ExUnit**: Comprehensive test coverage
- **@moduledoc/@doc**: Full documentation

## Complexity Reference

### Data Structures

| Operation | Stack | Queue | DynamicArray | LinkedList | HashTable | BST | Heap | DisjointSet |
|-----------|-------|-------|--------------|------------|-----------|-----|------|-------------|
| Access | O(n) | O(n) | O(n) | O(n) | O(1)* | O(log n)* | O(1) | - |
| Search | O(n) | O(n) | O(n) | O(n) | O(1)* | O(log n)* | O(n) | - |
| Insert | O(1) | O(1)* | O(n) | O(1)** | O(1)* | O(log n)* | O(log n) | O(α(n)) |
| Delete | O(1) | O(1)* | O(n) | O(1)** | O(1)* | O(log n)* | O(log n) | - |

\* Average case / amortized
\*\* At head; O(n) for arbitrary position

### Sorting Algorithms

| Algorithm | Best | Average | Worst | Space | Stable |
|-----------|------|---------|-------|-------|--------|
| InsertionSort | O(n) | O(n²) | O(n²) | O(n) | Yes |
| MergeSort | O(n log n) | O(n log n) | O(n log n) | O(n) | Yes |
| QuickSort | O(n log n) | O(n log n) | O(n²) | O(n) | No |
