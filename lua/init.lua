-- Data Structures and Algorithms Library for Lua
--
-- This library provides common data structures and algorithms
-- implemented in idiomatic Lua.

local DSA = {}

-- Data Structures
DSA.Stack = require("data_structures.stack")
DSA.Queue = require("data_structures.queue")
DSA.DynamicArray = require("data_structures.dynamic_array")
DSA.SinglyLinkedList = require("data_structures.singly_linked_list")
DSA.DoublyLinkedList = require("data_structures.doubly_linked_list")
DSA.Deque = require("data_structures.deque")
DSA.HashTable = require("data_structures.hash_table")
DSA.BinarySearchTree = require("data_structures.binary_search_tree")
DSA.MinHeap = require("data_structures.min_heap")
DSA.DisjointSet = require("data_structures.disjoint_set")

-- Algorithms
DSA.binary_search = require("algorithms.binary_search")
DSA.insertion_sort = require("algorithms.insertion_sort")
DSA.merge_sort = require("algorithms.merge_sort")
DSA.quick_sort = require("algorithms.quick_sort")
DSA.Graph = require("algorithms.graph")
DSA.bfs = require("algorithms.bfs")
DSA.dfs = require("algorithms.dfs")

return DSA
