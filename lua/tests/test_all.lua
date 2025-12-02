-- Comprehensive tests for DSA library

package.path = package.path .. ";../?.lua;../data_structures/?.lua;../algorithms/?.lua"

local Stack = require("data_structures.stack")
local Queue = require("data_structures.queue")
local DynamicArray = require("data_structures.dynamic_array")
local SinglyLinkedList = require("data_structures.singly_linked_list")
local DoublyLinkedList = require("data_structures.doubly_linked_list")
local Deque = require("data_structures.deque")
local HashTable = require("data_structures.hash_table")
local BinarySearchTree = require("data_structures.binary_search_tree")
local MinHeap = require("data_structures.min_heap")
local DisjointSet = require("data_structures.disjoint_set")
local binary_search = require("algorithms.binary_search")
local insertion_sort = require("algorithms.insertion_sort")
local merge_sort = require("algorithms.merge_sort")
local quick_sort = require("algorithms.quick_sort")
local Graph = require("algorithms.graph")
local bfs = require("algorithms.bfs")
local dfs = require("algorithms.dfs")

local function assertEquals(expected, actual, msg)
    if expected ~= actual then
        error(string.format("%s: expected %s, got %s", msg or "Assertion failed", tostring(expected), tostring(actual)))
    end
end

local function assertArrayEquals(expected, actual, msg)
    if #expected ~= #actual then
        error(string.format("%s: arrays have different lengths", msg or "Assertion failed"))
    end
    for i = 1, #expected do
        if expected[i] ~= actual[i] then
            error(string.format("%s: arrays differ at index %d", msg or "Assertion failed", i))
        end
    end
end

local function assertTrue(value, msg)
    if not value then
        error(msg or "Expected true")
    end
end

local function assertFalse(value, msg)
    if value then
        error(msg or "Expected false")
    end
end

-- Stack Tests
print("Testing Stack...")
local s = Stack.new()
assertTrue(s:isEmpty())
s:push(1)
s:push(2)
s:push(3)
assertFalse(s:isEmpty())
assertEquals(3, s:size())
assertEquals(3, s:pop())
assertEquals(2, s:peek())
assertEquals(2, s:size())
print("Stack tests passed!")

-- Queue Tests
print("Testing Queue...")
local q = Queue.new()
assertTrue(q:isEmpty())
q:enqueue(1)
q:enqueue(2)
q:enqueue(3)
assertFalse(q:isEmpty())
assertEquals(3, q:size())
assertEquals(1, q:dequeue())
assertEquals(2, q:peek())
assertEquals(2, q:size())
print("Queue tests passed!")

-- DynamicArray Tests
print("Testing DynamicArray...")
local arr = DynamicArray.new()
arr:push(1)
arr:push(2)
arr:push(3)
assertEquals(1, arr:get(1))
assertEquals(2, arr:get(2))
arr:set(2, 5)
assertEquals(5, arr:get(2))
assertEquals(3, arr:pop())
assertEquals(2, arr:size())
arr:insert(2, 10)
assertEquals(10, arr:get(2))
assertEquals(10, arr:removeAt(2))
print("DynamicArray tests passed!")

-- SinglyLinkedList Tests
print("Testing SinglyLinkedList...")
local sll = SinglyLinkedList.new()
sll:append(2)
sll:prepend(1)
sll:append(3)
assertEquals(1, sll:get(1))
assertEquals(2, sll:get(2))
assertEquals(3, sll:get(3))
assertEquals(3, sll:size())
assertEquals(1, sll:removeFirst())
assertEquals(2, sll:size())
print("SinglyLinkedList tests passed!")

-- DoublyLinkedList Tests
print("Testing DoublyLinkedList...")
local dll = DoublyLinkedList.new()
dll:append(2)
dll:prepend(1)
dll:append(3)
assertEquals(1, dll:get(1))
assertEquals(2, dll:get(2))
assertEquals(3, dll:get(3))
assertEquals(1, dll:removeFirst())
assertEquals(3, dll:removeLast())
assertEquals(1, dll:size())
print("DoublyLinkedList tests passed!")

-- Deque Tests
print("Testing Deque...")
local d = Deque.new()
d:pushBack(2)
d:pushFront(1)
d:pushBack(3)
assertEquals(1, d:peekFront())
assertEquals(3, d:peekBack())
assertEquals(1, d:popFront())
assertEquals(3, d:popBack())
assertEquals(1, d:size())
print("Deque tests passed!")

-- HashTable Tests
print("Testing HashTable...")
local ht = HashTable.new()
ht:put("one", 1)
ht:put("two", 2)
assertEquals(1, ht:get("one"))
assertEquals(2, ht:get("two"))
assertTrue(ht:contains("one"))
assertFalse(ht:contains("three"))
assertTrue(ht:remove("one"))
assertFalse(ht:contains("one"))
print("HashTable tests passed!")

-- BinarySearchTree Tests
print("Testing BinarySearchTree...")
local bst = BinarySearchTree.new()
bst:insert(5)
bst:insert(3)
bst:insert(7)
bst:insert(1)
bst:insert(9)
assertTrue(bst:contains(5))
assertTrue(bst:contains(3))
assertFalse(bst:contains(6))
assertEquals(1, bst:findMin())
assertEquals(9, bst:findMax())
assertArrayEquals({1, 3, 5, 7, 9}, bst:inorder())
assertTrue(bst:remove(3))
assertFalse(bst:contains(3))
print("BinarySearchTree tests passed!")

-- MinHeap Tests
print("Testing MinHeap...")
local heap = MinHeap.new()
heap:insert(5)
heap:insert(3)
heap:insert(7)
heap:insert(1)
assertEquals(1, heap:peek())
assertEquals(1, heap:extractMin())
assertEquals(3, heap:extractMin())
assertEquals(5, heap:extractMin())
assertEquals(7, heap:extractMin())

local heap2 = MinHeap.heapify({5, 3, 7, 1, 9})
assertEquals(1, heap2:peek())
print("MinHeap tests passed!")

-- DisjointSet Tests
print("Testing DisjointSet...")
local ds = DisjointSet.new()
ds:makeSet(1)
ds:makeSet(2)
ds:makeSet(3)
ds:makeSet(4)
assertFalse(ds:connected(1, 2))
ds:union(1, 2)
assertTrue(ds:connected(1, 2))
ds:union(3, 4)
assertTrue(ds:connected(3, 4))
assertFalse(ds:connected(1, 3))
ds:union(2, 3)
assertTrue(ds:connected(1, 4))
print("DisjointSet tests passed!")

-- BinarySearch Tests
print("Testing BinarySearch...")
local sortedArr = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
assertEquals(5, binary_search.search(sortedArr, 5))
assertEquals(1, binary_search.search(sortedArr, 1))
assertEquals(10, binary_search.search(sortedArr, 10))
assertEquals(nil, binary_search.search(sortedArr, 11))

local arr2 = {1, 2, 2, 2, 3, 4, 5}
assertEquals(2, binary_search.lowerBound(arr2, 2))
assertEquals(5, binary_search.upperBound(arr2, 2))
print("BinarySearch tests passed!")

-- InsertionSort Tests
print("Testing InsertionSort...")
local toSort = {5, 2, 8, 1, 9, 3}
insertion_sort.sort(toSort)
assertArrayEquals({1, 2, 3, 5, 8, 9}, toSort)

local toSort2 = {5, 2, 8, 1, 9, 3}
insertion_sort.sortDesc(toSort2)
assertArrayEquals({9, 8, 5, 3, 2, 1}, toSort2)
print("InsertionSort tests passed!")

-- MergeSort Tests
print("Testing MergeSort...")
local toSort3 = {5, 2, 8, 1, 9, 3, 7, 4, 6}
merge_sort.sort(toSort3)
assertArrayEquals({1, 2, 3, 4, 5, 6, 7, 8, 9}, toSort3)
print("MergeSort tests passed!")

-- QuickSort Tests
print("Testing QuickSort...")
local toSort4 = {5, 2, 8, 1, 9, 3, 7, 4, 6}
quick_sort.sort(toSort4)
assertArrayEquals({1, 2, 3, 4, 5, 6, 7, 8, 9}, toSort4)
print("QuickSort tests passed!")

-- Graph Tests
print("Testing Graph...")
local g = Graph.new(false)
g:addEdge(1, 2)
g:addEdge(1, 3)
assertTrue(g:hasVertex(1))
assertTrue(g:hasVertex(2))
assertTrue(g:hasEdge(1, 2))
assertTrue(g:hasEdge(2, 1))

local dg = Graph.new(true)
dg:addEdge(1, 2)
assertTrue(dg:hasEdge(1, 2))
assertFalse(dg:hasEdge(2, 1))
print("Graph tests passed!")

-- BFS Tests
print("Testing BFS...")
local g2 = Graph.new(false)
g2:addEdge(1, 2)
g2:addEdge(1, 3)
g2:addEdge(2, 4)

local order = bfs.traverse(g2, 1)
assertEquals(1, order[1])
assertEquals(4, #order)

local path = bfs.shortestPath(g2, 1, 4)
assertEquals(1, path[1])
assertEquals(4, path[#path])
assertEquals(3, #path)
print("BFS tests passed!")

-- DFS Tests
print("Testing DFS...")
local g3 = Graph.new(false)
g3:addEdge(1, 2)
g3:addEdge(1, 3)
g3:addEdge(2, 4)

local order2 = dfs.traverse(g3, 1)
assertEquals(1, order2[1])
assertEquals(4, #order2)

local cyclic = Graph.new(true)
cyclic:addEdge(1, 2)
cyclic:addEdge(2, 3)
cyclic:addEdge(3, 1)
assertTrue(dfs.hasCycle(cyclic))

local acyclic = Graph.new(true)
acyclic:addEdge(1, 2)
acyclic:addEdge(2, 3)
assertFalse(dfs.hasCycle(acyclic))

local sorted = dfs.topologicalSort(acyclic)
assertEquals(3, #sorted)
print("DFS tests passed!")

print("")
print("=================================")
print("All tests passed!")
print("=================================")
