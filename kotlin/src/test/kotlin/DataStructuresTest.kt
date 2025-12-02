import datastructures.*
import kotlin.test.*

class DataStructuresTest {
    // Stack Tests
    @Test
    fun `stack operations`() {
        val stack = Stack<Int>()
        assertTrue(stack.isEmpty())

        stack.push(1)
        stack.push(2)
        stack.push(3)

        assertEquals(3, stack.size())
        assertEquals(3, stack.peek())
        assertEquals(3, stack.pop())
        assertEquals(2, stack.pop())
        assertEquals(1, stack.size())

        stack.clear()
        assertTrue(stack.isEmpty())
    }

    // Queue Tests
    @Test
    fun `queue operations`() {
        val queue = Queue<Int>()
        assertTrue(queue.isEmpty())

        queue.enqueue(1)
        queue.enqueue(2)
        queue.enqueue(3)

        assertEquals(3, queue.size())
        assertEquals(1, queue.peek())
        assertEquals(1, queue.dequeue())
        assertEquals(2, queue.dequeue())
        assertEquals(1, queue.size())
    }

    // DynamicArray Tests
    @Test
    fun `dynamic array operations`() {
        val arr = DynamicArray<Int>()
        assertTrue(arr.isEmpty())

        arr.push(10)
        arr.push(20)
        arr.push(30)

        assertEquals(3, arr.size())
        assertEquals(10, arr.get(0))
        assertEquals(30, arr.get(2))

        arr.set(1, 25)
        assertEquals(25, arr.get(1))

        assertEquals(30, arr.pop())
        assertEquals(2, arr.size())

        arr.insert(1, 15)
        assertEquals(listOf(10, 15, 25), arr.toList())

        arr.removeAt(1)
        assertEquals(listOf(10, 25), arr.toList())
    }

    // SinglyLinkedList Tests
    @Test
    fun `singly linked list operations`() {
        val list = SinglyLinkedList<Int>()
        assertTrue(list.isEmpty())

        list.append(1)
        list.append(2)
        list.prepend(0)

        assertEquals(3, list.size())
        assertEquals(0, list.get(0))
        assertEquals(2, list.get(2))

        list.insert(1, 10)
        assertEquals(listOf(0, 10, 1, 2), list.toList())

        list.removeAt(1)
        assertEquals(listOf(0, 1, 2), list.toList())

        assertTrue(list.contains(1))
        assertFalse(list.contains(100))
    }

    // DoublyLinkedList Tests
    @Test
    fun `doubly linked list operations`() {
        val list = DoublyLinkedList<Int>()
        assertTrue(list.isEmpty())

        list.append(1)
        list.append(2)
        list.prepend(0)

        assertEquals(3, list.size())
        assertEquals(0, list.getFirst())
        assertEquals(2, list.getLast())

        assertEquals(0, list.removeFirst())
        assertEquals(2, list.removeLast())
        assertEquals(1, list.size())
    }

    // Deque Tests
    @Test
    fun `deque operations`() {
        val deque = Deque<Int>()
        assertTrue(deque.isEmpty())

        deque.pushBack(2)
        deque.pushFront(1)
        deque.pushBack(3)

        assertEquals(3, deque.size())
        assertEquals(1, deque.peekFront())
        assertEquals(3, deque.peekBack())

        assertEquals(1, deque.popFront())
        assertEquals(3, deque.popBack())
        assertEquals(1, deque.size())
    }

    // HashTable Tests
    @Test
    fun `hash table operations`() {
        val table = HashTable<String, Int>()
        assertTrue(table.isEmpty())

        table.put("one", 1)
        table.put("two", 2)
        table.put("three", 3)

        assertEquals(3, table.size())
        assertEquals(1, table.get("one"))
        assertEquals(2, table.get("two"))
        assertNull(table.get("four"))

        assertTrue(table.contains("one"))
        assertFalse(table.contains("four"))

        table.put("one", 100)
        assertEquals(100, table.get("one"))

        assertEquals(2, table.remove("two"))
        assertEquals(2, table.size())
        assertFalse(table.contains("two"))
    }

    // BinarySearchTree Tests
    @Test
    fun `binary search tree operations`() {
        val bst = BinarySearchTree<Int>()
        assertTrue(bst.isEmpty())

        bst.insert(5)
        bst.insert(3)
        bst.insert(7)
        bst.insert(1)
        bst.insert(9)

        assertEquals(5, bst.size())
        assertTrue(bst.contains(5))
        assertTrue(bst.contains(3))
        assertFalse(bst.contains(100))

        assertEquals(1, bst.min())
        assertEquals(9, bst.max())

        assertEquals(listOf(1, 3, 5, 7, 9), bst.inOrder())

        assertTrue(bst.remove(3))
        assertFalse(bst.contains(3))
        assertEquals(listOf(1, 5, 7, 9), bst.inOrder())
    }

    // MinHeap Tests
    @Test
    fun `min heap operations`() {
        val heap = MinHeap<Int>()
        assertTrue(heap.isEmpty())

        heap.insert(5)
        heap.insert(3)
        heap.insert(7)
        heap.insert(1)
        heap.insert(9)

        assertEquals(5, heap.size())
        assertEquals(1, heap.peek())

        assertEquals(1, heap.extractMin())
        assertEquals(3, heap.extractMin())
        assertEquals(5, heap.extractMin())
        assertEquals(7, heap.extractMin())
        assertEquals(9, heap.extractMin())

        assertTrue(heap.isEmpty())
    }

    @Test
    fun `min heap with custom comparator`() {
        // Max heap using custom comparator
        val heap = MinHeap<Int>(compareByDescending { it })

        heap.insert(5)
        heap.insert(3)
        heap.insert(7)

        assertEquals(7, heap.extractMin())
        assertEquals(5, heap.extractMin())
        assertEquals(3, heap.extractMin())
    }

    // DisjointSet Tests
    @Test
    fun `disjoint set operations`() {
        val ds = DisjointSet(5)

        assertEquals(5, ds.count())
        assertFalse(ds.connected(0, 1))

        ds.union(0, 1)
        assertTrue(ds.connected(0, 1))
        assertEquals(4, ds.count())

        ds.union(2, 3)
        ds.union(0, 2)

        assertTrue(ds.connected(0, 3))
        assertTrue(ds.connected(1, 2))
        assertEquals(2, ds.count())

        assertFalse(ds.connected(0, 4))
    }
}
