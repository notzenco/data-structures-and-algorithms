# C# Data Structures and Algorithms

This directory contains C# (.NET 8.0) implementations of common data structures and algorithms.

## Project Structure

```
csharp/
├── DSA.sln                    # Visual Studio solution
├── src/
│   ├── DSA.csproj             # Main library project
│   ├── DataStructures/
│   │   ├── Stack.cs
│   │   ├── Queue.cs
│   │   ├── DynamicArray.cs
│   │   ├── SinglyLinkedList.cs
│   │   ├── DoublyLinkedList.cs
│   │   ├── Deque.cs
│   │   ├── HashTable.cs
│   │   ├── BinarySearchTree.cs
│   │   ├── MinHeap.cs
│   │   └── DisjointSet.cs
│   └── Algorithms/
│       ├── BinarySearch.cs
│       ├── InsertionSort.cs
│       ├── MergeSort.cs
│       ├── QuickSort.cs
│       ├── Graph.cs
│       ├── BFS.cs
│       └── DFS.cs
└── tests/
    ├── DSA.Tests.csproj
    ├── DataStructuresTests.cs
    └── AlgorithmsTests.cs
```

## Requirements

- .NET 8.0 SDK or higher

## Building

```bash
dotnet build
```

## Running Tests

```bash
dotnet test
```

## Data Structures

| Structure | Description | Operations |
|-----------|-------------|------------|
| Stack | LIFO stack using linked nodes | Push, Pop, Peek, IsEmpty, Count |
| Queue | FIFO queue using linked nodes | Enqueue, Dequeue, Peek, IsEmpty, Count |
| DynamicArray | Growable array with amortized O(1) append | Push, Pop, Get, Set, RemoveAt, Insert |
| SinglyLinkedList | Single-direction linked list | PushFront, PushBack, PopFront, Contains, Remove |
| DoublyLinkedList | Bidirectional linked list | PushFront, PushBack, PopFront, PopBack, Remove |
| Deque | Double-ended queue | PushFront, PushBack, PopFront, PopBack, Peek |
| HashTable | Open addressing hash table with linear probing | Put, Get, Remove, ContainsKey |
| BinarySearchTree | BST with traversal methods | Insert, Remove, Contains, Min, Max, InOrder |
| MinHeap | Binary min-heap using List | Insert, ExtractMin, Peek |
| DisjointSet | Union-Find with path compression and union by rank | Find, Union, Connected, Count |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| Binary Search | Search in sorted array with bounds | O(log n) |
| Insertion Sort | Stable in-place sort | O(n²) average, O(n) best |
| Merge Sort | Stable divide-and-conquer sort | O(n log n) |
| Quick Sort | In-place partition sort with median-of-three | O(n log n) average |
| BFS | Breadth-first graph traversal | O(V + E) |
| DFS | Depth-first graph traversal | O(V + E) |

## Usage Examples

### Stack

```csharp
using DSA.DataStructures;

var stack = new Stack<int>();
stack.Push(1);
stack.Push(2);
stack.Push(3);

Console.WriteLine(stack.Pop());  // 3
Console.WriteLine(stack.Peek()); // 2
```

### HashTable

```csharp
using DSA.DataStructures;

var table = new HashTable<string, int>();
table.Put("one", 1);
table.Put("two", 2);

Console.WriteLine(table.Get("one"));      // 1
Console.WriteLine(table.ContainsKey("three")); // False
```

### Sorting

```csharp
using DSA.Algorithms;

var arr = new List<int> { 5, 2, 8, 1, 9 };
QuickSort.Sort(arr);
Console.WriteLine(string.Join(", ", arr)); // 1, 2, 5, 8, 9

// With custom comparer
var words = new List<string> { "banana", "apple", "cherry" };
MergeSort.Sort(words, StringComparer.Ordinal);
```

### Graph Algorithms

```csharp
using DSA.Algorithms;

var graph = new Graph<string>();
graph.AddEdge("A", "B");
graph.AddEdge("A", "C");
graph.AddEdge("B", "D");
graph.AddEdge("C", "D");

// BFS traversal
var bfsResult = BFS.Traverse(graph, "A");
Console.WriteLine(string.Join(", ", bfsResult)); // A, B, C, D

// Find shortest path
var path = BFS.FindPath(graph, "A", "D");
Console.WriteLine(string.Join(" -> ", path!)); // A -> B -> D

// Get distances from source
var distances = BFS.Distances(graph, "A");

// DFS with cycle detection (for directed graphs)
var directed = new Graph<string>(directed: true);
directed.AddEdge("A", "B");
directed.AddEdge("B", "C");
directed.AddEdge("C", "A");
Console.WriteLine(DFS.HasCycle(directed, "A")); // True
```

## Design Notes

- Uses nullable reference types for safety
- Generic implementations with IComparer<T> support
- Modern C# features (pattern matching, tuples, records)
- All collections implement standard operations with consistent naming
