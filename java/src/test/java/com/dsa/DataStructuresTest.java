package com.dsa;

import com.dsa.datastructures.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Optional;

class DataStructuresTest {

    // Stack Tests
    @Test
    void stackBasicOperations() {
        Stack<Integer> stack = new Stack<>();
        assertTrue(stack.isEmpty());
        assertEquals(0, stack.size());

        stack.push(1);
        stack.push(2);
        stack.push(3);

        assertEquals(3, stack.size());
        assertFalse(stack.isEmpty());
        assertEquals(Optional.of(3), stack.peek());
        assertEquals(Optional.of(3), stack.pop());
        assertEquals(Optional.of(2), stack.pop());
        assertEquals(2, stack.size());
    }

    @Test
    void stackEmptyOperations() {
        Stack<String> stack = new Stack<>();
        assertEquals(Optional.empty(), stack.pop());
        assertEquals(Optional.empty(), stack.peek());
    }

    // Queue Tests
    @Test
    void queueBasicOperations() {
        Queue<Integer> queue = new Queue<>();
        assertTrue(queue.isEmpty());

        queue.enqueue(1);
        queue.enqueue(2);
        queue.enqueue(3);

        assertEquals(3, queue.size());
        assertEquals(Optional.of(1), queue.peek());
        assertEquals(Optional.of(1), queue.dequeue());
        assertEquals(Optional.of(2), queue.dequeue());
    }

    @Test
    void queueEmptyOperations() {
        Queue<String> queue = new Queue<>();
        assertEquals(Optional.empty(), queue.dequeue());
        assertEquals(Optional.empty(), queue.peek());
    }

    // DynamicArray Tests
    @Test
    void dynamicArrayBasicOperations() {
        DynamicArray<Integer> arr = new DynamicArray<>();
        assertTrue(arr.isEmpty());

        arr.push(10);
        arr.push(20);
        arr.push(30);

        assertEquals(3, arr.size());
        assertEquals(Optional.of(10), arr.get(0));
        assertEquals(Optional.of(30), arr.get(2));
    }

    @Test
    void dynamicArraySetAndRemove() {
        DynamicArray<String> arr = new DynamicArray<>();
        arr.push("a");
        arr.push("b");
        arr.push("c");

        arr.set(1, "x");
        assertEquals(Optional.of("x"), arr.get(1));

        assertEquals(Optional.of("x"), arr.remove(1));
        assertEquals(2, arr.size());
        assertEquals(Optional.of("c"), arr.get(1));
    }

    @Test
    void dynamicArrayGrowth() {
        DynamicArray<Integer> arr = new DynamicArray<>(2);
        for (int i = 0; i < 100; i++) {
            arr.push(i);
        }
        assertEquals(100, arr.size());
        assertEquals(Optional.of(99), arr.get(99));
    }

    // SinglyLinkedList Tests
    @Test
    void singlyLinkedListBasicOperations() {
        SinglyLinkedList<Integer> list = new SinglyLinkedList<>();
        assertTrue(list.isEmpty());

        list.pushFront(1);
        list.pushFront(2);
        list.pushBack(3);

        assertEquals(3, list.size());
        assertEquals(Optional.of(2), list.peekFront());
        assertEquals(Optional.of(3), list.peekBack());
    }

    @Test
    void singlyLinkedListFind() {
        SinglyLinkedList<String> list = new SinglyLinkedList<>();
        list.pushBack("a");
        list.pushBack("b");
        list.pushBack("c");

        assertTrue(list.contains("b"));
        assertFalse(list.contains("d"));
    }

    // DoublyLinkedList Tests
    @Test
    void doublyLinkedListBasicOperations() {
        DoublyLinkedList<Integer> list = new DoublyLinkedList<>();
        assertTrue(list.isEmpty());

        list.pushFront(1);
        list.pushFront(2);
        list.pushBack(3);

        assertEquals(3, list.size());
        assertEquals(Optional.of(2), list.popFront());
        assertEquals(Optional.of(3), list.popBack());
    }

    @Test
    void doublyLinkedListRemove() {
        DoublyLinkedList<String> list = new DoublyLinkedList<>();
        list.pushBack("a");
        list.pushBack("b");
        list.pushBack("c");

        assertTrue(list.remove("b"));
        assertEquals(2, list.size());
        assertFalse(list.contains("b"));
    }

    // Deque Tests
    @Test
    void dequeBasicOperations() {
        Deque<Integer> deque = new Deque<>();
        assertTrue(deque.isEmpty());

        deque.pushFront(1);
        deque.pushBack(2);
        deque.pushFront(0);

        assertEquals(3, deque.size());
        assertEquals(Optional.of(0), deque.peekFront());
        assertEquals(Optional.of(2), deque.peekBack());
    }

    @Test
    void dequeMixedOperations() {
        Deque<String> deque = new Deque<>();
        deque.pushBack("a");
        deque.pushBack("b");
        deque.pushFront("x");

        assertEquals(Optional.of("x"), deque.popFront());
        assertEquals(Optional.of("b"), deque.popBack());
        assertEquals(Optional.of("a"), deque.popFront());
        assertTrue(deque.isEmpty());
    }

    // HashTable Tests
    @Test
    void hashTableBasicOperations() {
        HashTable<String, Integer> table = new HashTable<>();
        assertTrue(table.isEmpty());

        table.put("one", 1);
        table.put("two", 2);
        table.put("three", 3);

        assertEquals(3, table.size());
        assertEquals(Optional.of(1), table.get("one"));
        assertEquals(Optional.of(2), table.get("two"));
        assertTrue(table.containsKey("three"));
    }

    @Test
    void hashTableUpdateAndRemove() {
        HashTable<String, String> table = new HashTable<>();
        table.put("key", "value1");
        assertEquals(Optional.of("value1"), table.get("key"));

        table.put("key", "value2");
        assertEquals(Optional.of("value2"), table.get("key"));
        assertEquals(1, table.size());

        assertEquals(Optional.of("value2"), table.remove("key"));
        assertFalse(table.containsKey("key"));
    }

    @Test
    void hashTableCollisionHandling() {
        HashTable<Integer, String> table = new HashTable<>(4);
        for (int i = 0; i < 20; i++) {
            table.put(i, "value" + i);
        }
        assertEquals(20, table.size());
        for (int i = 0; i < 20; i++) {
            assertEquals(Optional.of("value" + i), table.get(i));
        }
    }

    // BinarySearchTree Tests
    @Test
    void bstBasicOperations() {
        BinarySearchTree<Integer> bst = new BinarySearchTree<>();
        assertTrue(bst.isEmpty());

        bst.insert(5);
        bst.insert(3);
        bst.insert(7);
        bst.insert(1);
        bst.insert(9);

        assertEquals(5, bst.size());
        assertTrue(bst.contains(3));
        assertTrue(bst.contains(7));
        assertFalse(bst.contains(4));
    }

    @Test
    void bstMinMax() {
        BinarySearchTree<Integer> bst = new BinarySearchTree<>();
        bst.insert(5);
        bst.insert(3);
        bst.insert(7);
        bst.insert(1);
        bst.insert(9);

        assertEquals(Optional.of(1), bst.min());
        assertEquals(Optional.of(9), bst.max());
    }

    @Test
    void bstInorderTraversal() {
        BinarySearchTree<Integer> bst = new BinarySearchTree<>();
        bst.insert(5);
        bst.insert(3);
        bst.insert(7);
        bst.insert(1);
        bst.insert(9);

        var inorder = bst.inorder();
        assertEquals(5, inorder.size());
        assertEquals(Integer.valueOf(1), inorder.get(0));
        assertEquals(Integer.valueOf(3), inorder.get(1));
        assertEquals(Integer.valueOf(5), inorder.get(2));
        assertEquals(Integer.valueOf(7), inorder.get(3));
        assertEquals(Integer.valueOf(9), inorder.get(4));
    }

    @Test
    void bstRemove() {
        BinarySearchTree<Integer> bst = new BinarySearchTree<>();
        bst.insert(5);
        bst.insert(3);
        bst.insert(7);

        assertTrue(bst.remove(3));
        assertFalse(bst.contains(3));
        assertEquals(2, bst.size());
    }

    // MinHeap Tests
    @Test
    void minHeapBasicOperations() {
        MinHeap<Integer> heap = new MinHeap<>();
        assertTrue(heap.isEmpty());

        heap.insert(5);
        heap.insert(3);
        heap.insert(7);
        heap.insert(1);

        assertEquals(4, heap.size());
        assertEquals(Optional.of(1), heap.peek());
        assertEquals(Optional.of(1), heap.extractMin());
        assertEquals(Optional.of(3), heap.peek());
    }

    @Test
    void minHeapMaintainsOrder() {
        MinHeap<Integer> heap = new MinHeap<>();
        int[] values = {9, 4, 7, 1, 8, 2, 6, 3, 5};
        for (int v : values) {
            heap.insert(v);
        }

        int prev = Integer.MIN_VALUE;
        while (!heap.isEmpty()) {
            int current = heap.extractMin().orElseThrow();
            assertTrue(current >= prev);
            prev = current;
        }
    }

    // DisjointSet Tests
    @Test
    void disjointSetBasicOperations() {
        DisjointSet ds = new DisjointSet(5);

        assertFalse(ds.connected(0, 1));
        ds.union(0, 1);
        assertTrue(ds.connected(0, 1));
    }

    @Test
    void disjointSetMultipleUnions() {
        DisjointSet ds = new DisjointSet(6);

        ds.union(0, 1);
        ds.union(2, 3);
        ds.union(4, 5);

        assertTrue(ds.connected(0, 1));
        assertTrue(ds.connected(2, 3));
        assertFalse(ds.connected(0, 2));

        ds.union(1, 2);
        assertTrue(ds.connected(0, 3));
    }

    @Test
    void disjointSetComponentCount() {
        DisjointSet ds = new DisjointSet(5);
        assertEquals(5, ds.count());

        ds.union(0, 1);
        assertEquals(4, ds.count());

        ds.union(2, 3);
        assertEquals(3, ds.count());

        ds.union(0, 2);
        assertEquals(2, ds.count());
    }
}
