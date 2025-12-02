package datastructures

/**
 * FIFO (First In, First Out) data structure using a linked list.
 * Time: O(1) for all operations
 * Space: O(n)
 */
class Queue<T> {
    private class Node<T>(val value: T, var next: Node<T>? = null)

    private var head: Node<T>? = null
    private var tail: Node<T>? = null
    private var count = 0

    fun enqueue(value: T) {
        val node = Node(value)
        if (tail != null) {
            tail!!.next = node
        } else {
            head = node
        }
        tail = node
        count++
    }

    fun dequeue(): T? {
        val node = head ?: return null
        head = node.next
        if (head == null) {
            tail = null
        }
        count--
        return node.value
    }

    fun peek(): T? = head?.value

    fun isEmpty(): Boolean = count == 0

    fun size(): Int = count

    fun clear() {
        head = null
        tail = null
        count = 0
    }
}
