# Data Structures and Algorithms - Julia

A comprehensive collection of data structures and algorithms implemented in Julia.

## Requirements

- Julia 1.6+

## Project Structure

```
julia/
├── Project.toml
├── src/
│   ├── DSA.jl
│   ├── data_structures/
│   │   ├── stack.jl
│   │   ├── queue.jl
│   │   ├── dynamic_array.jl
│   │   ├── singly_linked_list.jl
│   │   ├── doubly_linked_list.jl
│   │   ├── deque.jl
│   │   ├── hash_table.jl
│   │   ├── binary_search_tree.jl
│   │   ├── min_heap.jl
│   │   └── disjoint_set.jl
│   └── algorithms/
│       ├── binary_search.jl
│       ├── insertion_sort.jl
│       ├── merge_sort.jl
│       ├── quick_sort.jl
│       ├── graph.jl
│       ├── bfs.jl
│       └── dfs.jl
├── test/
│   └── runtests.jl
└── README.md
```

## Running Tests

```julia
# From the julia directory
julia --project=. test/runtests.jl

# Or using Pkg
using Pkg
Pkg.test()
```

## Data Structures

| Data Structure | Description | Operations |
|----------------|-------------|------------|
| Stack | LIFO container | push!, pop!, peek, isempty, size |
| Queue | FIFO container | enqueue!, dequeue!, peek, isempty |
| DynamicArray | Resizable array | push!, pop!, get, set!, insert!, remove_at! |
| SinglyLinkedList | Forward-only linked list | prepend!, append!, get, remove_at! |
| DoublyLinkedList | Bidirectional linked list | prepend!, append!, remove_first!, remove_last! |
| Deque | Double-ended queue | push_front!, push_back!, pop_front!, pop_back! |
| HashTable | Open addressing hash map | put!, get, remove!, contains |
| BinarySearchTree | Ordered binary tree | insert!, contains, find_min, find_max, inorder |
| MinHeap | Binary min heap | insert!, extract_min!, peek, heapify |
| DisjointSet | Union-Find | make_set!, find!, union!, connected |

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

```julia
include("src/DSA.jl")
using .DSA

s = Stack{Int}()
push!(s, 1)
push!(s, 2)
push!(s, 3)

println(pop!(s))   # 3
println(peek(s))   # 2
println(size(s))   # 2
```

### Queue

```julia
q = Queue{String}()
enqueue!(q, "first")
enqueue!(q, "second")

println(dequeue!(q))  # "first"
println(peek(q))      # "second"
```

### Hash Table

```julia
ht = HashTable{String, Int}()
ht["one"] = 1
ht["two"] = 2

println(ht["one"])           # 1
println(contains(ht, "two")) # true
```

### Binary Search Tree

```julia
tree = BinarySearchTree{Int}()
insert!(tree, 5)
insert!(tree, 3)
insert!(tree, 7)

println(contains(tree, 5))  # true
println(find_min(tree))     # 3
println(inorder(tree))      # [3, 5, 7]
```

### Sorting

```julia
arr = [5, 2, 8, 1, 9, 3]
quick_sort!(arr)
println(arr)  # [1, 2, 3, 5, 8, 9]

arr2 = [5, 2, 8, 1, 9, 3]
merge_sort!(arr2)
println(arr2)  # [1, 2, 3, 5, 8, 9]
```

### Graph Traversal

```julia
g = Graph{Int}(false)  # undirected
add_edge!(g, 1, 2)
add_edge!(g, 1, 3)
add_edge!(g, 2, 4)

order = bfs_traverse(g, 1)
println(order)  # [1, 2, 3, 4]

path = shortest_path(g, 1, 4)
println(path)  # [1, 2, 4]
```

## Julia Features

This implementation leverages several Julia features:

- **Parametric types**: Generic data structures using `{T}`
- **Multiple dispatch**: Overloading functions for different types
- **Mutating functions**: Convention of `!` suffix for in-place modifications
- **Union types**: `Union{T, Nothing}` for optional values
- **Structs**: Mutable and immutable composite types
- **Iterator protocol**: `iterate` method implementations

## Complexity Reference

| Operation | Stack | Queue | DynamicArray | LinkedList | HashTable | BST | Heap | DisjointSet |
|-----------|-------|-------|--------------|------------|-----------|-----|------|-------------|
| Access | O(n) | O(n) | O(1) | O(n) | O(1)* | O(log n)* | O(1) | - |
| Search | O(n) | O(n) | O(n) | O(n) | O(1)* | O(log n)* | O(n) | - |
| Insert | O(1)* | O(1)* | O(1)* | O(1) | O(1)* | O(log n)* | O(log n) | O(α(n)) |
| Delete | O(1) | O(1)* | O(n) | O(1) | O(1)* | O(log n)* | O(log n) | - |

\* Amortized / average case
