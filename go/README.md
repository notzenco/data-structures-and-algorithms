# Go Data Structures and Algorithms

This directory contains Go 1.21+ implementations of common data structures and algorithms using generics.

## Project Structure

```
go/
├── go.mod
├── datastructures/
│   ├── stack.go
│   ├── queue.go
│   ├── dynamic_array.go
│   ├── singly_linked_list.go
│   ├── doubly_linked_list.go
│   ├── deque.go
│   ├── hash_table.go
│   ├── binary_search_tree.go
│   ├── min_heap.go
│   ├── disjoint_set.go
│   └── datastructures_test.go
├── algorithms/
│   ├── binary_search.go
│   ├── insertion_sort.go
│   ├── merge_sort.go
│   ├── quick_sort.go
│   ├── graph.go
│   ├── bfs.go
│   ├── dfs.go
│   └── algorithms_test.go
└── README.md
```

## Requirements

- Go 1.21 or higher (for generics with cmp.Ordered)

## Running Tests

```bash
go test ./...
```

## Data Structures

| Structure | Description | Operations |
|-----------|-------------|------------|
| Stack | LIFO stack using linked nodes | Push, Pop, Peek, IsEmpty, Size |
| Queue | FIFO queue using linked nodes | Enqueue, Dequeue, Peek, IsEmpty, Size |
| DynamicArray | Growable array with amortized O(1) append | Push, Pop, Get, Set, Remove, Insert |
| SinglyLinkedList | Single-direction linked list | PushFront, PushBack, PopFront, Contains, Remove |
| DoublyLinkedList | Bidirectional linked list | PushFront, PushBack, PopFront, PopBack, Remove |
| Deque | Double-ended queue | PushFront, PushBack, PopFront, PopBack, Peek |
| HashTable | Open addressing hash table with linear probing | Put, Get, Remove, Contains |
| BinarySearchTree | BST with traversal methods | Insert, Remove, Contains, Min, Max, InOrder |
| MinHeap | Binary min-heap using slice | Insert, ExtractMin, Peek |
| DisjointSet | Union-Find with path compression and union by rank | Find, Union, Connected, Count |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| Binary Search | Search in sorted slice with bounds | O(log n) |
| Insertion Sort | Stable in-place sort | O(n²) average, O(n) best |
| Merge Sort | Stable divide-and-conquer sort | O(n log n) |
| Quick Sort | In-place partition sort with median-of-three | O(n log n) average |
| BFS | Breadth-first graph traversal | O(V + E) |
| DFS | Depth-first graph traversal | O(V + E) |

## Usage Examples

### Stack

```go
import "github.com/dsa/go/datastructures"

stack := datastructures.NewStack[int]()
stack.Push(1)
stack.Push(2)
stack.Push(3)

val, ok := stack.Pop()  // 3, true
val, ok = stack.Peek()  // 2, true
```

### Sorting

```go
import "github.com/dsa/go/algorithms"

arr := []int{5, 2, 8, 1, 9}
algorithms.QuickSort(arr)
fmt.Println(arr) // [1 2 5 8 9]

// With custom comparator
words := []string{"banana", "apple", "cherry"}
algorithms.QuickSortFunc(words, func(a, b string) bool {
    return a < b
})
```

### Graph Algorithms

```go
import "github.com/dsa/go/algorithms"

graph := algorithms.NewGraph[string](false) // undirected
graph.AddEdge("A", "B")
graph.AddEdge("A", "C")
graph.AddEdge("B", "D")
graph.AddEdge("C", "D")

// BFS traversal
result := algorithms.BFS(graph, "A", nil)
fmt.Println(result) // [A B C D]

// Find shortest path
path := algorithms.BFSFindPath(graph, "A", "D")
fmt.Println(path) // [A B D] or [A C D]

// Get distances from source
distances := algorithms.BFSDistances(graph, "A")
fmt.Println(distances) // map[A:0 B:1 C:1 D:2]

// Cycle detection (for directed graphs)
directed := algorithms.NewGraph[string](true)
directed.AddEdge("A", "B")
directed.AddEdge("B", "C")
directed.AddEdge("C", "A")
fmt.Println(algorithms.HasCycle(directed, "A")) // true
```

## Design Notes

- Uses Go 1.21+ generics with type constraints (cmp.Ordered, comparable)
- Idiomatic Go with multiple return values (value, ok)
- Custom comparator functions for sorting (SortFunc variants)
- Graph supports both directed and undirected modes
