import XCTest
@testable import DSA

final class DataStructuresTests: XCTestCase {
    // MARK: - Stack Tests
    func testStackOperations() {
        var stack = Stack<Int>()
        XCTAssertTrue(stack.isEmpty)

        stack.push(1)
        stack.push(2)
        stack.push(3)

        XCTAssertEqual(stack.count, 3)
        XCTAssertEqual(stack.peek(), 3)
        XCTAssertEqual(stack.pop(), 3)
        XCTAssertEqual(stack.pop(), 2)
        XCTAssertEqual(stack.count, 1)

        stack.clear()
        XCTAssertTrue(stack.isEmpty)
    }

    // MARK: - Queue Tests
    func testQueueOperations() {
        var queue = Queue<Int>()
        XCTAssertTrue(queue.isEmpty)

        queue.enqueue(1)
        queue.enqueue(2)
        queue.enqueue(3)

        XCTAssertEqual(queue.count, 3)
        XCTAssertEqual(queue.peek(), 1)
        XCTAssertEqual(queue.dequeue(), 1)
        XCTAssertEqual(queue.dequeue(), 2)
        XCTAssertEqual(queue.count, 1)
    }

    // MARK: - DynamicArray Tests
    func testDynamicArrayOperations() {
        var arr = DynamicArray<Int>()
        XCTAssertTrue(arr.isEmpty)

        arr.push(10)
        arr.push(20)
        arr.push(30)

        XCTAssertEqual(arr.count, 3)
        XCTAssertEqual(arr.get(0), 10)
        XCTAssertEqual(arr.get(2), 30)

        XCTAssertTrue(arr.set(1, 25))
        XCTAssertEqual(arr.get(1), 25)

        XCTAssertEqual(arr.pop(), 30)
        XCTAssertEqual(arr.count, 2)

        XCTAssertTrue(arr.insert(1, 15))
        XCTAssertEqual(arr.toArray(), [10, 15, 25])

        XCTAssertEqual(arr.removeAt(1), 15)
        XCTAssertEqual(arr.toArray(), [10, 25])
    }

    // MARK: - SinglyLinkedList Tests
    func testSinglyLinkedListOperations() {
        var list = SinglyLinkedList<Int>()
        XCTAssertTrue(list.isEmpty)

        list.append(1)
        list.append(2)
        list.prepend(0)

        XCTAssertEqual(list.count, 3)
        XCTAssertEqual(list.get(0), 0)
        XCTAssertEqual(list.get(2), 2)

        XCTAssertTrue(list.insert(1, 10))
        XCTAssertEqual(list.toArray(), [0, 10, 1, 2])

        XCTAssertEqual(list.removeAt(1), 10)
        XCTAssertEqual(list.toArray(), [0, 1, 2])

        XCTAssertTrue(list.contains(1))
        XCTAssertFalse(list.contains(100))
    }

    // MARK: - DoublyLinkedList Tests
    func testDoublyLinkedListOperations() {
        var list = DoublyLinkedList<Int>()
        XCTAssertTrue(list.isEmpty)

        list.append(1)
        list.append(2)
        list.prepend(0)

        XCTAssertEqual(list.count, 3)
        XCTAssertEqual(list.getFirst(), 0)
        XCTAssertEqual(list.getLast(), 2)

        XCTAssertEqual(list.removeFirst(), 0)
        XCTAssertEqual(list.removeLast(), 2)
        XCTAssertEqual(list.count, 1)
    }

    // MARK: - Deque Tests
    func testDequeOperations() {
        var deque = Deque<Int>()
        XCTAssertTrue(deque.isEmpty)

        deque.pushBack(2)
        deque.pushFront(1)
        deque.pushBack(3)

        XCTAssertEqual(deque.count, 3)
        XCTAssertEqual(deque.peekFront(), 1)
        XCTAssertEqual(deque.peekBack(), 3)

        XCTAssertEqual(deque.popFront(), 1)
        XCTAssertEqual(deque.popBack(), 3)
        XCTAssertEqual(deque.count, 1)
    }

    // MARK: - HashTable Tests
    func testHashTableOperations() {
        var table = HashTable<String, Int>()
        XCTAssertTrue(table.isEmpty)

        table.put("one", 1)
        table.put("two", 2)
        table.put("three", 3)

        XCTAssertEqual(table.count, 3)
        XCTAssertEqual(table.get("one"), 1)
        XCTAssertEqual(table.get("two"), 2)
        XCTAssertNil(table.get("four"))

        XCTAssertTrue(table.contains("one"))
        XCTAssertFalse(table.contains("four"))

        table.put("one", 100)
        XCTAssertEqual(table.get("one"), 100)

        XCTAssertEqual(table.remove("two"), 2)
        XCTAssertEqual(table.count, 2)
        XCTAssertFalse(table.contains("two"))
    }

    // MARK: - BinarySearchTree Tests
    func testBinarySearchTreeOperations() {
        var bst = BinarySearchTree<Int>()
        XCTAssertTrue(bst.isEmpty)

        bst.insert(5)
        bst.insert(3)
        bst.insert(7)
        bst.insert(1)
        bst.insert(9)

        XCTAssertEqual(bst.count, 5)
        XCTAssertTrue(bst.contains(5))
        XCTAssertTrue(bst.contains(3))
        XCTAssertFalse(bst.contains(100))

        XCTAssertEqual(bst.min(), 1)
        XCTAssertEqual(bst.max(), 9)

        XCTAssertEqual(bst.inOrder(), [1, 3, 5, 7, 9])

        XCTAssertTrue(bst.remove(3))
        XCTAssertFalse(bst.contains(3))
        XCTAssertEqual(bst.inOrder(), [1, 5, 7, 9])
    }

    // MARK: - MinHeap Tests
    func testMinHeapOperations() {
        var heap = MinHeap<Int>()
        XCTAssertTrue(heap.isEmpty)

        heap.insert(5)
        heap.insert(3)
        heap.insert(7)
        heap.insert(1)
        heap.insert(9)

        XCTAssertEqual(heap.count, 5)
        XCTAssertEqual(heap.peek(), 1)

        XCTAssertEqual(heap.extractMin(), 1)
        XCTAssertEqual(heap.extractMin(), 3)
        XCTAssertEqual(heap.extractMin(), 5)
        XCTAssertEqual(heap.extractMin(), 7)
        XCTAssertEqual(heap.extractMin(), 9)

        XCTAssertTrue(heap.isEmpty)
    }

    func testMinHeapWithCustomComparator() {
        // Max heap using custom comparator
        var heap = MinHeap<Int> { $0 > $1 ? -1 : ($0 < $1 ? 1 : 0) }

        heap.insert(5)
        heap.insert(3)
        heap.insert(7)

        XCTAssertEqual(heap.extractMin(), 7)
        XCTAssertEqual(heap.extractMin(), 5)
        XCTAssertEqual(heap.extractMin(), 3)
    }

    // MARK: - DisjointSet Tests
    func testDisjointSetOperations() {
        var ds = DisjointSet(5)

        XCTAssertEqual(ds.count, 5)
        XCTAssertFalse(ds.connected(0, 1))

        XCTAssertTrue(ds.union(0, 1))
        XCTAssertTrue(ds.connected(0, 1))
        XCTAssertEqual(ds.count, 4)

        XCTAssertTrue(ds.union(2, 3))
        XCTAssertTrue(ds.union(0, 2))

        XCTAssertTrue(ds.connected(0, 3))
        XCTAssertTrue(ds.connected(1, 2))
        XCTAssertEqual(ds.count, 2)

        XCTAssertFalse(ds.connected(0, 4))
    }
}
