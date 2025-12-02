cmake_minimum_required(VERSION 3.14)
project(dsa_cpp VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Compiler warnings
if(MSVC)
    add_compile_options(/W4 /permissive-)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# Testing
enable_testing()

# -----------------------------------------------------------------------------
# Data Structures
# -----------------------------------------------------------------------------

# Stack
add_executable(stack_test data_structures/stack/stack_test.cpp)
target_include_directories(stack_test PRIVATE data_structures/stack)
add_test(NAME stack_test COMMAND stack_test)

# Queue
add_executable(queue_test data_structures/queue/queue_test.cpp)
target_include_directories(queue_test PRIVATE data_structures/queue)
add_test(NAME queue_test COMMAND queue_test)

# Dynamic Array
add_executable(dynamic_array_test data_structures/dynamic-array/dynamic_array_test.cpp)
target_include_directories(dynamic_array_test PRIVATE data_structures/dynamic-array)
add_test(NAME dynamic_array_test COMMAND dynamic_array_test)

# Singly Linked List
add_executable(singly_linked_list_test data_structures/singly-linked-list/singly_linked_list_test.cpp)
target_include_directories(singly_linked_list_test PRIVATE data_structures/singly-linked-list)
add_test(NAME singly_linked_list_test COMMAND singly_linked_list_test)

# Doubly Linked List
add_executable(doubly_linked_list_test data_structures/doubly-linked-list/doubly_linked_list_test.cpp)
target_include_directories(doubly_linked_list_test PRIVATE data_structures/doubly-linked-list)
add_test(NAME doubly_linked_list_test COMMAND doubly_linked_list_test)

# Deque
add_executable(deque_test data_structures/deque/deque_test.cpp)
target_include_directories(deque_test PRIVATE data_structures/deque)
add_test(NAME deque_test COMMAND deque_test)

# Hash Table
add_executable(hash_table_test data_structures/hash-table/hash_table_test.cpp)
target_include_directories(hash_table_test PRIVATE data_structures/hash-table)
add_test(NAME hash_table_test COMMAND hash_table_test)

# Binary Search Tree
add_executable(bst_test data_structures/binary-search-tree/bst_test.cpp)
target_include_directories(bst_test PRIVATE data_structures/binary-search-tree)
add_test(NAME bst_test COMMAND bst_test)

# Binary Min Heap
add_executable(heap_test data_structures/heap-binary-min/heap_test.cpp)
target_include_directories(heap_test PRIVATE data_structures/heap-binary-min)
add_test(NAME heap_test COMMAND heap_test)

# Disjoint Set
add_executable(disjoint_set_test data_structures/disjoint-set/disjoint_set_test.cpp)
target_include_directories(disjoint_set_test PRIVATE data_structures/disjoint-set)
add_test(NAME disjoint_set_test COMMAND disjoint_set_test)

# -----------------------------------------------------------------------------
# Algorithms - Searching
# -----------------------------------------------------------------------------

# Binary Search
add_executable(binary_search_test algorithms/searching/binary-search/binary_search_test.cpp)
target_include_directories(binary_search_test PRIVATE algorithms/searching/binary-search)
add_test(NAME binary_search_test COMMAND binary_search_test)

# -----------------------------------------------------------------------------
# Algorithms - Sorting
# -----------------------------------------------------------------------------

# Insertion Sort
add_executable(insertion_sort_test algorithms/sorting/insertion-sort/insertion_sort_test.cpp)
target_include_directories(insertion_sort_test PRIVATE algorithms/sorting/insertion-sort)
add_test(NAME insertion_sort_test COMMAND insertion_sort_test)

# Merge Sort
add_executable(merge_sort_test algorithms/sorting/merge-sort/merge_sort_test.cpp)
target_include_directories(merge_sort_test PRIVATE algorithms/sorting/merge-sort)
add_test(NAME merge_sort_test COMMAND merge_sort_test)

# Quick Sort
add_executable(quick_sort_test algorithms/sorting/quick-sort/quick_sort_test.cpp)
target_include_directories(quick_sort_test PRIVATE algorithms/sorting/quick-sort)
add_test(NAME quick_sort_test COMMAND quick_sort_test)

# -----------------------------------------------------------------------------
# Algorithms - Graph
# -----------------------------------------------------------------------------

# BFS
add_executable(bfs_test algorithms/graph/bfs/bfs_test.cpp)
target_include_directories(bfs_test PRIVATE algorithms/graph/bfs)
add_test(NAME bfs_test COMMAND bfs_test)

# DFS
add_executable(dfs_test algorithms/graph/dfs/dfs_test.cpp)
target_include_directories(dfs_test PRIVATE algorithms/graph/dfs)
add_test(NAME dfs_test COMMAND dfs_test)
