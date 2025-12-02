# Java Data Structures and Algorithms

This directory contains Java 17 implementations of common data structures and algorithms.

## Project Structure

```
java/
├── pom.xml                           # Maven configuration
├── src/
│   ├── main/java/com/dsa/
│   │   ├── datastructures/           # Data structure implementations
│   │   │   ├── Stack.java
│   │   │   ├── Queue.java
│   │   │   ├── DynamicArray.java
│   │   │   ├── SinglyLinkedList.java
│   │   │   ├── DoublyLinkedList.java
│   │   │   ├── Deque.java
│   │   │   ├── HashTable.java
│   │   │   ├── BinarySearchTree.java
│   │   │   ├── MinHeap.java
│   │   │   └── DisjointSet.java
│   │   └── algorithms/               # Algorithm implementations
│   │       ├── BinarySearch.java
│   │       ├── InsertionSort.java
│   │       ├── MergeSort.java
│   │       ├── QuickSort.java
│   │       ├── Graph.java
│   │       ├── BFS.java
│   │       └── DFS.java
│   └── test/java/com/dsa/
│       ├── DataStructuresTest.java
│       └── AlgorithmsTest.java
└── README.md
```

## Requirements

- Java 17 or higher
- Maven 3.6 or higher

## Building

```bash
mvn compile
```

## Running Tests

```bash
mvn test
```

## Data Structures

| Structure | Description | Operations |
|-----------|-------------|------------|
| Stack | LIFO stack using linked nodes | push, pop, peek, isEmpty, size |
| Queue | FIFO queue using linked nodes | enqueue, dequeue, peek, isEmpty, size |
| DynamicArray | Growable array with amortized O(1) append | push, pop, get, set, remove, insert |
| SinglyLinkedList | Single-direction linked list | pushFront, pushBack, popFront, contains, remove |
| DoublyLinkedList | Bidirectional linked list | pushFront, pushBack, popFront, popBack, remove |
| Deque | Double-ended queue | pushFront, pushBack, popFront, popBack, peek |
| HashTable | Open addressing hash table with linear probing | put, get, remove, containsKey |
| BinarySearchTree | BST with in-order traversal | insert, remove, contains, min, max, inorder |
| MinHeap | Binary min-heap using array | insert, extractMin, peek, decreaseKey |
| DisjointSet | Union-Find with path compression and union by rank | find, union, connected, count |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| Binary Search | Search in sorted list | O(log n) |
| Insertion Sort | Stable in-place sort | O(n²) average, O(n) best |
| Merge Sort | Stable divide-and-conquer sort | O(n log n) |
| Quick Sort | In-place partition sort with median-of-three | O(n log n) average |
| BFS | Breadth-first graph traversal | O(V + E) |
| DFS | Depth-first graph traversal | O(V + E) |

## Usage Examples

### Stack

```java
import com.dsa.datastructures.Stack;

Stack<Integer> stack = new Stack<>();
stack.push(1);
stack.push(2);
stack.push(3);

System.out.println(stack.pop());  // Optional[3]
System.out.println(stack.peek()); // Optional[2]
```

### HashTable

```java
import com.dsa.datastructures.HashTable;

HashTable<String, Integer> table = new HashTable<>();
table.put("one", 1);
table.put("two", 2);

System.out.println(table.get("one")); // Optional[1]
System.out.println(table.containsKey("three")); // false
```

### Sorting

```java
import com.dsa.algorithms.QuickSort;
import java.util.*;

List<Integer> arr = new ArrayList<>(Arrays.asList(5, 2, 8, 1, 9));
QuickSort.sort(arr);
System.out.println(arr); // [1, 2, 5, 8, 9]

// With custom comparator
List<String> words = new ArrayList<>(Arrays.asList("banana", "apple", "cherry"));
QuickSort.sort(words, String::compareTo);
```

### Graph Algorithms

```java
import com.dsa.algorithms.*;

Graph<String> graph = new Graph<>();
graph.addEdge("A", "B");
graph.addEdge("A", "C");
graph.addEdge("B", "D");
graph.addEdge("C", "D");

// BFS traversal
List<String> bfsResult = BFS.traverse(graph, "A");
System.out.println(bfsResult); // [A, B, C, D]

// Find shortest path
Optional<List<String>> path = BFS.findPath(graph, "A", "D");
System.out.println(path); // Optional[[A, B, D]] or Optional[[A, C, D]]

// Get distances from source
Map<String, Integer> distances = BFS.distances(graph, "A");
System.out.println(distances); // {A=0, B=1, C=1, D=2}

// DFS with cycle detection (for directed graphs)
Graph<String> directed = new Graph<>(true);
directed.addEdge("A", "B");
directed.addEdge("B", "C");
directed.addEdge("C", "A");
System.out.println(DFS.hasCycle(directed, "A")); // true
```

## Design Notes

- All data structures use Java generics for type safety
- Optional<T> is used for operations that might not return a value
- Comparator<T> support allows custom comparison logic for sorting and searching
- Graph implementation supports both directed and undirected graphs
- Algorithms are implemented as static utility methods for easy use
