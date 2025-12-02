# Data Structures and Algorithms - Haskell

A comprehensive collection of purely functional data structures and algorithms implemented in Haskell.

## Requirements

- GHC 9.0+
- Cabal 3.0+ or Stack

## Project Structure

```
haskell/
├── dsa-haskell.cabal
├── package.yaml
├── src/
│   ├── DataStructures/
│   │   ├── Stack.hs
│   │   ├── Queue.hs
│   │   ├── DynamicArray.hs
│   │   ├── SinglyLinkedList.hs
│   │   ├── DoublyLinkedList.hs
│   │   ├── Deque.hs
│   │   ├── HashTable.hs
│   │   ├── BinarySearchTree.hs
│   │   ├── MinHeap.hs
│   │   └── DisjointSet.hs
│   └── Algorithms/
│       ├── BinarySearch.hs
│       ├── InsertionSort.hs
│       ├── MergeSort.hs
│       ├── QuickSort.hs
│       ├── Graph.hs
│       ├── BFS.hs
│       └── DFS.hs
├── test/
│   ├── Spec.hs
│   ├── DataStructuresSpec.hs
│   └── AlgorithmsSpec.hs
└── README.md
```

## Building and Testing

```bash
# Using Cabal
cabal build
cabal test

# Using Stack
stack build
stack test

# Run GHCi for interactive testing
cabal repl
# or
stack ghci
```

## Data Structures

| Data Structure | Description | Operations |
|----------------|-------------|------------|
| Stack | LIFO container (list-based) | push, pop, peek, isEmpty, size |
| Queue | FIFO container (banker's queue) | enqueue, dequeue, peek, isEmpty, size |
| DynamicArray | Vector-backed array | push, pop, get, set, insert, removeAt |
| SinglyLinkedList | Forward-only linked list | prepend, append, insert, removeAt, get |
| DoublyLinkedList | Bidirectional list | prepend, append, removeFirst, removeLast |
| Deque | Double-ended queue | pushFront, pushBack, popFront, popBack |
| HashTable | HashMap wrapper | put, get, remove, contains, keys, values |
| BinarySearchTree | Ordered binary tree | insert, remove, contains, findMin, findMax |
| MinHeap | Leftist heap | insert, extractMin, peek |
| DisjointSet | Union-Find with path compression | makeSet, find, union, connected |

## Algorithms

| Algorithm | Description | Time Complexity |
|-----------|-------------|-----------------|
| BinarySearch | Search in sorted Vector | O(log n) |
| InsertionSort | Simple stable sort | O(n²) |
| MergeSort | Divide-and-conquer sort | O(n log n) |
| QuickSort | Median-of-three pivot | O(n log n) avg |
| Graph | Adjacency list representation | - |
| BFS | Breadth-first search | O(V + E) |
| DFS | Depth-first search | O(V + E) |

## Usage Examples

### Stack

```haskell
import qualified DataStructures.Stack as Stack

main = do
  let stack = Stack.push 3 $ Stack.push 2 $ Stack.push 1 Stack.empty
  print $ Stack.peek stack          -- Just 3
  case Stack.pop stack of
    Just (val, rest) -> print val   -- 3
    Nothing -> print "empty"
```

### BinarySearchTree

```haskell
import qualified DataStructures.BinarySearchTree as BST

main = do
  let bst = BST.fromList [5, 3, 7, 1, 9]
  print $ BST.inorder bst           -- [1, 3, 5, 7, 9]
  print $ BST.findMin bst           -- Just 1
  print $ BST.contains 5 bst        -- True
```

### MinHeap

```haskell
import qualified DataStructures.MinHeap as Heap

main = do
  let heap = Heap.fromList [5, 3, 7, 1, 9]
  print $ Heap.toList heap          -- [1, 3, 5, 7, 9] (sorted)
  print $ Heap.peek heap            -- Just 1
```

### Sorting

```haskell
import qualified Algorithms.MergeSort as MS
import qualified Algorithms.QuickSort as QS

main = do
  let xs = [5, 2, 8, 1, 9, 3]
  print $ MS.sort xs                -- [1, 2, 3, 5, 8, 9]
  print $ QS.sort xs                -- [1, 2, 3, 5, 8, 9]
```

### Graph Traversals

```haskell
import qualified Algorithms.Graph as G
import qualified Algorithms.BFS as BFS
import qualified Algorithms.DFS as DFS

main = do
  let graph = G.addEdge 3 4 $ G.addEdge 2 4 $ G.addEdge 1 3 $ G.addEdge 1 2 G.undirected

  print $ BFS.traverse graph 1      -- [1, 2, 3, 4]
  print $ BFS.shortestPath graph 1 4 -- Just [1, 2, 4] or [1, 3, 4]
  print $ BFS.distances graph 1     -- Map with distances

  print $ DFS.traverse graph 1      -- [1, ...]
  print $ DFS.hasCycle graph        -- depends on graph
```

### DisjointSet

```haskell
import qualified DataStructures.DisjointSet as DS

main = do
  let ds = DS.makeSet 4 $ DS.makeSet 3 $ DS.makeSet 2 $ DS.makeSet 1 DS.empty

  print $ DS.connected 1 2 ds       -- False

  case DS.union 1 2 ds of
    Just ds' -> do
      print $ DS.connected 1 2 ds'  -- True
      print $ DS.setCount ds'       -- 3
    Nothing -> print "error"
```

## Functional Programming Notes

All implementations in this library are purely functional:

- **Immutability**: All data structures are immutable. Operations return new structures.
- **Persistent**: Previous versions remain accessible after modifications.
- **No side effects**: All functions are pure (except IO in tests).

### Implementation Techniques

- **Stack**: Simple list representation (cons is O(1))
- **Queue**: Banker's queue with two lists for amortized O(1) operations
- **MinHeap**: Leftist heap for efficient merging
- **DisjointSet**: Functional path compression (returns updated structure)

## Complexity Reference

### Data Structures

| Operation | Stack | Queue | DynamicArray | LinkedList | HashTable | BST | Heap | DisjointSet |
|-----------|-------|-------|--------------|------------|-----------|-----|------|-------------|
| Access | O(n) | O(n) | O(1) | O(n) | O(1)* | O(log n)* | O(1) | - |
| Search | O(n) | O(n) | O(n) | O(n) | O(1)* | O(log n)* | O(n) | - |
| Insert | O(1) | O(1)* | O(n) | O(1)** | O(1)* | O(log n)* | O(log n) | O(log n) |
| Delete | O(1) | O(1)* | O(n) | O(1)** | O(1)* | O(log n)* | O(log n) | - |

\* Average case / amortized
\*\* At head; O(n) for arbitrary position

Note: In a purely functional setting, "delete" typically means creating a new structure without the element.

### Sorting Algorithms

| Algorithm | Best | Average | Worst | Space | Stable |
|-----------|------|---------|-------|-------|--------|
| InsertionSort | O(n) | O(n²) | O(n²) | O(n) | Yes |
| MergeSort | O(n log n) | O(n log n) | O(n log n) | O(n) | Yes |
| QuickSort | O(n log n) | O(n log n) | O(n²) | O(n) | No |

Note: Functional implementations create new lists, hence O(n) space for all.
