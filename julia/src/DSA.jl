"""
Data Structures and Algorithms Library for Julia

This library provides common data structures and algorithms
implemented in idiomatic Julia.
"""
module DSA

# Data Structures
include("data_structures/stack.jl")
include("data_structures/queue.jl")
include("data_structures/dynamic_array.jl")
include("data_structures/singly_linked_list.jl")
include("data_structures/doubly_linked_list.jl")
include("data_structures/deque.jl")
include("data_structures/hash_table.jl")
include("data_structures/binary_search_tree.jl")
include("data_structures/min_heap.jl")
include("data_structures/disjoint_set.jl")

# Algorithms
include("algorithms/binary_search.jl")
include("algorithms/insertion_sort.jl")
include("algorithms/merge_sort.jl")
include("algorithms/quick_sort.jl")
include("algorithms/graph.jl")
include("algorithms/bfs.jl")
include("algorithms/dfs.jl")

# Export Data Structures
export Stack, push!, pop!, peek, isempty, size
export Queue, enqueue!, dequeue!
export DynamicArray, get, set!, insert!, remove_at!
export SinglyLinkedList, prepend!, append!, remove_first!
export DoublyLinkedList, remove_last!
export Deque, push_front!, push_back!, pop_front!, pop_back!, peek_front, peek_back
export HashTable, put!, get, remove!, contains, keys, values
export BinarySearchTree, insert!, contains, find_min, find_max, inorder
export MinHeap, extract_min!, heapify
export DisjointSet, make_set!, find!, union!, connected

# Export Algorithms
export binary_search, lower_bound, upper_bound
export insertion_sort!, merge_sort!, quick_sort!
export Graph, add_vertex!, add_edge!, has_vertex, has_edge, neighbors, vertices
export bfs_traverse, shortest_path, distances
export dfs_traverse, find_path, has_cycle, topological_sort

end # module
