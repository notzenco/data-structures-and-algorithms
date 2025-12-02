cmake_minimum_required(VERSION 3.14)
project(dsa_c VERSION 1.0.0 LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Compiler warnings
if(MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# Testing
enable_testing()

# -----------------------------------------------------------------------------
# Data Structures
# -----------------------------------------------------------------------------

# Stack
add_library(stack data_structures/stack/stack.c)
target_include_directories(stack PUBLIC data_structures/stack)

add_executable(stack_test data_structures/stack/stack_test.c)
target_link_libraries(stack_test PRIVATE stack)
add_test(NAME stack_test COMMAND stack_test)

# Queue
add_library(queue data_structures/queue/queue.c)
target_include_directories(queue PUBLIC data_structures/queue)

add_executable(queue_test data_structures/queue/queue_test.c)
target_link_libraries(queue_test PRIVATE queue)
add_test(NAME queue_test COMMAND queue_test)

# Dynamic Array
add_library(dynamic_array data_structures/dynamic-array/dynamic_array.c)
target_include_directories(dynamic_array PUBLIC data_structures/dynamic-array)

add_executable(dynamic_array_test data_structures/dynamic-array/dynamic_array_test.c)
target_link_libraries(dynamic_array_test PRIVATE dynamic_array)
add_test(NAME dynamic_array_test COMMAND dynamic_array_test)

# Singly Linked List
add_library(singly_linked_list data_structures/singly-linked-list/singly_linked_list.c)
target_include_directories(singly_linked_list PUBLIC data_structures/singly-linked-list)

add_executable(singly_linked_list_test data_structures/singly-linked-list/singly_linked_list_test.c)
target_link_libraries(singly_linked_list_test PRIVATE singly_linked_list)
add_test(NAME singly_linked_list_test COMMAND singly_linked_list_test)

# Doubly Linked List
add_library(doubly_linked_list data_structures/doubly-linked-list/doubly_linked_list.c)
target_include_directories(doubly_linked_list PUBLIC data_structures/doubly-linked-list)

add_executable(doubly_linked_list_test data_structures/doubly-linked-list/doubly_linked_list_test.c)
target_link_libraries(doubly_linked_list_test PRIVATE doubly_linked_list)
add_test(NAME doubly_linked_list_test COMMAND doubly_linked_list_test)

# Deque
add_library(deque data_structures/deque/deque.c)
target_include_directories(deque PUBLIC data_structures/deque)

add_executable(deque_test data_structures/deque/deque_test.c)
target_link_libraries(deque_test PRIVATE deque)
add_test(NAME deque_test COMMAND deque_test)

# Hash Table
add_library(hash_table data_structures/hash-table-open-addressing/hash_table.c)
target_include_directories(hash_table PUBLIC data_structures/hash-table-open-addressing)

add_executable(hash_table_test data_structures/hash-table-open-addressing/hash_table_test.c)
target_link_libraries(hash_table_test PRIVATE hash_table)
add_test(NAME hash_table_test COMMAND hash_table_test)

# Binary Search Tree
add_library(bst data_structures/binary-search-tree/bst.c)
target_include_directories(bst PUBLIC data_structures/binary-search-tree)

add_executable(bst_test data_structures/binary-search-tree/bst_test.c)
target_link_libraries(bst_test PRIVATE bst)
add_test(NAME bst_test COMMAND bst_test)

# Binary Min Heap
add_library(heap data_structures/heap-binary-min/heap.c)
target_include_directories(heap PUBLIC data_structures/heap-binary-min)

add_executable(heap_test data_structures/heap-binary-min/heap_test.c)
target_link_libraries(heap_test PRIVATE heap)
add_test(NAME heap_test COMMAND heap_test)

# Disjoint Set
add_library(disjoint_set data_structures/disjoint-set/disjoint_set.c)
target_include_directories(disjoint_set PUBLIC data_structures/disjoint-set)

add_executable(disjoint_set_test data_structures/disjoint-set/disjoint_set_test.c)
target_link_libraries(disjoint_set_test PRIVATE disjoint_set)
add_test(NAME disjoint_set_test COMMAND disjoint_set_test)
