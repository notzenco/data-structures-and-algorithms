## Comprehensive tests for DSA library

import std/unittest
import std/sequtils
import ../src/dsa

suite "Stack Tests":
  test "push and pop":
    var s = newStack[int]()
    s.push(1)
    s.push(2)
    s.push(3)
    check s.pop() == 3
    check s.pop() == 2
    check s.pop() == 1

  test "peek":
    var s = newStack[int]()
    s.push(42)
    check s.peek() == 42
    check s.size() == 1

  test "isEmpty":
    var s = newStack[int]()
    check s.isEmpty()
    s.push(1)
    check not s.isEmpty()

suite "Queue Tests":
  test "enqueue and dequeue":
    var q = newQueue[int]()
    q.enqueue(1)
    q.enqueue(2)
    q.enqueue(3)
    check q.dequeue() == 1
    check q.dequeue() == 2
    check q.dequeue() == 3

  test "peek":
    var q = newQueue[int]()
    q.enqueue(42)
    check q.peek() == 42
    check q.size() == 1

suite "DynamicArray Tests":
  test "push and get":
    var arr = newDynamicArray[int]()
    arr.push(1)
    arr.push(2)
    arr.push(3)
    check arr[0] == 1
    check arr[1] == 2
    check arr[2] == 3

  test "set":
    var arr = newDynamicArray[int]()
    arr.push(1)
    arr[0] = 42
    check arr[0] == 42

  test "insert and removeAt":
    var arr = newDynamicArray[int]()
    arr.push(1)
    arr.push(3)
    arr.insert(1, 2)
    check arr[1] == 2
    check arr.removeAt(1) == 2
    check arr.size() == 2

suite "SinglyLinkedList Tests":
  test "prepend and append":
    var list = newSinglyLinkedList[int]()
    list.append(2)
    list.prepend(1)
    list.append(3)
    check list[0] == 1
    check list[1] == 2
    check list[2] == 3

  test "removeFirst":
    var list = newSinglyLinkedList[int]()
    list.append(1)
    list.append(2)
    check list.removeFirst() == 1
    check list.size() == 1

suite "DoublyLinkedList Tests":
  test "prepend and append":
    var list = newDoublyLinkedList[int]()
    list.append(2)
    list.prepend(1)
    list.append(3)
    check list[0] == 1
    check list[1] == 2
    check list[2] == 3

  test "removeFirst and removeLast":
    var list = newDoublyLinkedList[int]()
    list.append(1)
    list.append(2)
    list.append(3)
    check list.removeFirst() == 1
    check list.removeLast() == 3
    check list.size() == 1

suite "Deque Tests":
  test "pushFront and pushBack":
    var d = newDeque[int]()
    d.pushBack(2)
    d.pushFront(1)
    d.pushBack(3)
    check d.peekFront() == 1
    check d.peekBack() == 3

  test "popFront and popBack":
    var d = newDeque[int]()
    d.pushBack(1)
    d.pushBack(2)
    d.pushBack(3)
    check d.popFront() == 1
    check d.popBack() == 3

suite "HashTable Tests":
  test "put and get":
    var ht = newHashTable[string, int]()
    ht["one"] = 1
    ht["two"] = 2
    check ht["one"] == 1
    check ht["two"] == 2

  test "contains and remove":
    var ht = newHashTable[string, int]()
    ht["key"] = 42
    check ht.contains("key")
    check ht.remove("key")
    check not ht.contains("key")

suite "BinarySearchTree Tests":
  test "insert and contains":
    var tree = newBinarySearchTree[int]()
    tree.insert(5)
    tree.insert(3)
    tree.insert(7)
    check tree.contains(5)
    check tree.contains(3)
    check tree.contains(7)
    check not tree.contains(4)

  test "findMin and findMax":
    var tree = newBinarySearchTree[int]()
    tree.insert(5)
    tree.insert(3)
    tree.insert(7)
    tree.insert(1)
    tree.insert(9)
    check tree.findMin() == 1
    check tree.findMax() == 9

  test "remove":
    var tree = newBinarySearchTree[int]()
    tree.insert(5)
    tree.insert(3)
    tree.insert(7)
    check tree.remove(3)
    check not tree.contains(3)

  test "inorder":
    var tree = newBinarySearchTree[int]()
    tree.insert(5)
    tree.insert(3)
    tree.insert(7)
    tree.insert(1)
    tree.insert(9)
    check tree.inorder() == @[1, 3, 5, 7, 9]

suite "MinHeap Tests":
  test "insert and extractMin":
    var heap = newMinHeap[int]()
    heap.insert(5)
    heap.insert(3)
    heap.insert(7)
    heap.insert(1)
    check heap.extractMin() == 1
    check heap.extractMin() == 3
    check heap.extractMin() == 5
    check heap.extractMin() == 7

  test "peek":
    var heap = newMinHeap[int]()
    heap.insert(5)
    heap.insert(3)
    check heap.peek() == 3
    check heap.size() == 2

  test "heapify":
    var heap = heapify(@[5, 3, 7, 1, 9])
    check heap.extractMin() == 1

suite "DisjointSet Tests":
  test "makeSet and find":
    var ds = newDisjointSet[int]()
    ds.makeSet(1)
    ds.makeSet(2)
    check ds.find(1) == 1
    check ds.find(2) == 2

  test "union and connected":
    var ds = newDisjointSet[int]()
    ds.makeSet(1)
    ds.makeSet(2)
    ds.makeSet(3)
    check not ds.connected(1, 2)
    discard ds.union(1, 2)
    check ds.connected(1, 2)
    check not ds.connected(1, 3)

suite "BinarySearch Tests":
  test "binarySearch":
    let arr = @[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    check binarySearch(arr, 5) == 4
    check binarySearch(arr, 1) == 0
    check binarySearch(arr, 10) == 9
    check binarySearch(arr, 11) == -1

  test "lowerBound and upperBound":
    let arr = @[1, 2, 2, 2, 3, 4, 5]
    check lowerBound(arr, 2) == 1
    check upperBound(arr, 2) == 4

suite "InsertionSort Tests":
  test "sort":
    var arr = @[5, 2, 8, 1, 9, 3]
    insertionSort(arr)
    check arr == @[1, 2, 3, 5, 8, 9]

  test "sortDesc":
    var arr = @[5, 2, 8, 1, 9, 3]
    insertionSortDesc(arr)
    check arr == @[9, 8, 5, 3, 2, 1]

suite "MergeSort Tests":
  test "sort":
    var arr = @[5, 2, 8, 1, 9, 3, 7, 4, 6]
    mergeSort(arr)
    check arr == @[1, 2, 3, 4, 5, 6, 7, 8, 9]

  test "sortIterative":
    var arr = @[5, 2, 8, 1, 9, 3, 7, 4, 6]
    mergeSortIterative(arr)
    check arr == @[1, 2, 3, 4, 5, 6, 7, 8, 9]

suite "QuickSort Tests":
  test "sort":
    var arr = @[5, 2, 8, 1, 9, 3, 7, 4, 6]
    quickSort(arr)
    check arr == @[1, 2, 3, 4, 5, 6, 7, 8, 9]

  test "quickSelect":
    var arr = @[5, 2, 8, 1, 9]
    check quickSelect(arr, 0) == 1
    arr = @[5, 2, 8, 1, 9]
    check quickSelect(arr, 4) == 9

suite "Graph Tests":
  test "addEdge and hasEdge":
    var g = newGraph[int](directed = false)
    g.addEdge(1, 2)
    g.addEdge(1, 3)
    check g.hasEdge(1, 2)
    check g.hasEdge(2, 1)
    check g.hasEdge(1, 3)

  test "directed graph":
    var g = newGraph[int](directed = true)
    g.addEdge(1, 2)
    check g.hasEdge(1, 2)
    check not g.hasEdge(2, 1)

suite "BFS Tests":
  test "traverse":
    var g = newGraph[int](directed = false)
    g.addEdge(1, 2)
    g.addEdge(1, 3)
    g.addEdge(2, 4)
    let order = bfs.traverse(g, 1)
    check order[0] == 1
    check order.len == 4

  test "shortestPath":
    var g = newGraph[int](directed = false)
    g.addEdge(1, 2)
    g.addEdge(2, 3)
    g.addEdge(1, 3)
    let path = bfs.shortestPath(g, 1, 3)
    check path == @[1, 3]

suite "DFS Tests":
  test "traverse":
    var g = newGraph[int](directed = false)
    g.addEdge(1, 2)
    g.addEdge(1, 3)
    g.addEdge(2, 4)
    let order = dfs.traverse(g, 1)
    check order[0] == 1
    check order.len == 4

  test "hasCycle":
    var cyclic = newGraph[int](directed = true)
    cyclic.addEdge(1, 2)
    cyclic.addEdge(2, 3)
    cyclic.addEdge(3, 1)
    check dfs.hasCycle(cyclic)

    var acyclic = newGraph[int](directed = true)
    acyclic.addEdge(1, 2)
    acyclic.addEdge(2, 3)
    check not dfs.hasCycle(acyclic)

  test "topologicalSort":
    var g = newGraph[int](directed = true)
    g.addEdge(1, 2)
    g.addEdge(1, 3)
    g.addEdge(2, 3)
    let sorted = dfs.topologicalSort(g)
    check sorted.len == 3
    check sorted.find(1) < sorted.find(2)
    check sorted.find(1) < sorted.find(3)
    check sorted.find(2) < sorted.find(3)

echo "All tests completed!"
