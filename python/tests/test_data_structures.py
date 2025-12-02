"""Tests for data structures."""

import pytest
from dsa import (
    Stack, Queue, DynamicArray, SinglyLinkedList, DoublyLinkedList,
    Deque, HashTable, BinarySearchTree, MinHeap, DisjointSet
)


class TestStack:
    def test_push_pop(self):
        stack = Stack()
        stack.push(1)
        stack.push(2)
        assert stack.pop() == 2
        assert stack.pop() == 1
        assert stack.pop() is None

    def test_peek(self):
        stack = Stack()
        assert stack.peek() is None
        stack.push(42)
        assert stack.peek() == 42
        assert len(stack) == 1

    def test_is_empty(self):
        stack = Stack()
        assert stack.is_empty()
        stack.push(1)
        assert not stack.is_empty()


class TestQueue:
    def test_enqueue_dequeue(self):
        queue = Queue()
        queue.enqueue(1)
        queue.enqueue(2)
        assert queue.dequeue() == 1
        assert queue.dequeue() == 2
        assert queue.dequeue() is None

    def test_fifo_order(self):
        queue = Queue()
        for i in range(5):
            queue.enqueue(i)
        for i in range(5):
            assert queue.dequeue() == i


class TestDynamicArray:
    def test_append_pop(self):
        arr = DynamicArray()
        arr.append(1)
        arr.append(2)
        assert arr.pop() == 2
        assert len(arr) == 1

    def test_insert_remove(self):
        arr = DynamicArray()
        arr.append(1)
        arr.append(3)
        arr.insert(1, 2)
        assert arr.get(1) == 2
        assert arr.remove(1) == 2

    def test_indexing(self):
        arr = DynamicArray()
        arr.append(10)
        assert arr[0] == 10
        arr[0] = 20
        assert arr[0] == 20


class TestSinglyLinkedList:
    def test_push_pop_front(self):
        lst = SinglyLinkedList()
        lst.push_front(1)
        lst.push_front(2)
        assert lst.pop_front() == 2
        assert lst.pop_front() == 1

    def test_push_back(self):
        lst = SinglyLinkedList()
        lst.push_back(1)
        lst.push_back(2)
        assert lst.pop_front() == 1

    def test_contains(self):
        lst = SinglyLinkedList()
        lst.push_back(1)
        lst.push_back(2)
        assert lst.contains(2)
        assert not lst.contains(3)


class TestDoublyLinkedList:
    def test_push_pop_front(self):
        lst = DoublyLinkedList()
        lst.push_front(1)
        lst.push_front(2)
        assert lst.pop_front() == 2

    def test_push_pop_back(self):
        lst = DoublyLinkedList()
        lst.push_back(1)
        lst.push_back(2)
        assert lst.pop_back() == 2

    def test_mixed_operations(self):
        lst = DoublyLinkedList()
        lst.push_back(1)
        lst.push_front(0)
        lst.push_back(2)
        assert lst.pop_front() == 0
        assert lst.pop_back() == 2


class TestDeque:
    def test_push_pop_front(self):
        d = Deque()
        d.push_front(1)
        d.push_front(2)
        assert d.pop_front() == 2

    def test_push_pop_back(self):
        d = Deque()
        d.push_back(1)
        d.push_back(2)
        assert d.pop_back() == 2

    def test_get(self):
        d = Deque()
        d.push_back(10)
        d.push_back(20)
        assert d.get(0) == 10
        assert d.get(1) == 20


class TestHashTable:
    def test_insert_get(self):
        ht = HashTable()
        ht.insert("key1", "value1")
        ht.insert("key2", "value2")
        assert ht.get("key1") == "value1"
        assert ht.get("key3") is None

    def test_update(self):
        ht = HashTable()
        ht.insert("key", "value1")
        old = ht.insert("key", "value2")
        assert old == "value1"
        assert ht.get("key") == "value2"

    def test_remove(self):
        ht = HashTable()
        ht.insert("key", "value")
        assert ht.remove("key") == "value"
        assert ht.get("key") is None

    def test_contains(self):
        ht = HashTable()
        ht.insert(1, "one")
        assert ht.contains(1)
        assert "1" not in ht


class TestBinarySearchTree:
    def test_insert_contains(self):
        bst = BinarySearchTree()
        assert bst.insert(5)
        assert bst.insert(3)
        assert bst.insert(7)
        assert not bst.insert(5)  # Duplicate
        assert bst.contains(5)
        assert not bst.contains(1)

    def test_remove(self):
        bst = BinarySearchTree()
        bst.insert(5)
        bst.insert(3)
        bst.insert(7)
        assert bst.remove(3)
        assert not bst.contains(3)

    def test_min_max(self):
        bst = BinarySearchTree()
        bst.insert(5)
        bst.insert(3)
        bst.insert(7)
        assert bst.min() == 3
        assert bst.max() == 7

    def test_inorder(self):
        bst = BinarySearchTree()
        for v in [5, 3, 7, 1, 9]:
            bst.insert(v)
        assert bst.inorder() == [1, 3, 5, 7, 9]


class TestMinHeap:
    def test_push_pop(self):
        heap = MinHeap()
        heap.push(5)
        heap.push(3)
        heap.push(7)
        heap.push(1)
        assert heap.pop() == 1
        assert heap.pop() == 3

    def test_peek(self):
        heap = MinHeap()
        assert heap.peek() is None
        heap.push(5)
        heap.push(3)
        assert heap.peek() == 3

    def test_heapify(self):
        heap = MinHeap.heapify([5, 3, 7, 1, 9])
        assert heap.peek() == 1


class TestDisjointSet:
    def test_initial_state(self):
        ds = DisjointSet(5)
        assert ds.num_sets == 5
        assert len(ds) == 5

    def test_union_find(self):
        ds = DisjointSet(5)
        assert ds.union(0, 1)
        assert ds.connected(0, 1)
        assert not ds.connected(0, 2)
        assert ds.num_sets == 4

    def test_set_size(self):
        ds = DisjointSet(5)
        ds.union(0, 1)
        ds.union(0, 2)
        assert ds.set_size(0) == 3
        assert ds.set_size(1) == 3
