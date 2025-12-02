pub const data_structures = struct {
    pub const Stack = @import("data_structures/stack.zig").Stack;
    pub const Queue = @import("data_structures/queue.zig").Queue;
    pub const DynamicArray = @import("data_structures/dynamic_array.zig").DynamicArray;
    pub const SinglyLinkedList = @import("data_structures/singly_linked_list.zig").SinglyLinkedList;
    pub const DoublyLinkedList = @import("data_structures/doubly_linked_list.zig").DoublyLinkedList;
    pub const Deque = @import("data_structures/deque.zig").Deque;
    pub const HashTable = @import("data_structures/hash_table.zig").HashTable;
    pub const BinarySearchTree = @import("data_structures/binary_search_tree.zig").BinarySearchTree;
    pub const MinHeap = @import("data_structures/min_heap.zig").MinHeap;
    pub const DisjointSet = @import("data_structures/disjoint_set.zig").DisjointSet;
};

pub const algorithms = struct {
    pub const binary_search = @import("algorithms/binary_search.zig");
    pub const insertion_sort = @import("algorithms/insertion_sort.zig");
    pub const merge_sort = @import("algorithms/merge_sort.zig");
    pub const quick_sort = @import("algorithms/quick_sort.zig");
    pub const Graph = @import("algorithms/graph.zig").Graph;
    pub const bfs = @import("algorithms/bfs.zig");
    pub const dfs = @import("algorithms/dfs.zig");
};

test {
    @import("std").testing.refAllDecls(@This());
}
