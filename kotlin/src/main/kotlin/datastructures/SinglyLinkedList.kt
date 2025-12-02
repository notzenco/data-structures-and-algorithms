package datastructures

/**
 * Singly linked list with forward traversal.
 * Time: O(1) for prepend, O(n) for other operations
 * Space: O(n)
 */
class SinglyLinkedList<T> {
    private class Node<T>(var value: T, var next: Node<T>? = null)

    private var head: Node<T>? = null
    private var count = 0

    fun prepend(value: T) {
        head = Node(value, head)
        count++
    }

    fun append(value: T) {
        val node = Node(value)
        if (head == null) {
            head = node
        } else {
            var current = head
            while (current!!.next != null) {
                current = current.next
            }
            current.next = node
        }
        count++
    }

    fun insert(index: Int, value: T): Boolean {
        if (index < 0 || index > count) return false
        if (index == 0) {
            prepend(value)
            return true
        }
        var current = head
        for (i in 0 until index - 1) {
            current = current?.next
        }
        current?.next = Node(value, current?.next)
        count++
        return true
    }

    fun removeAt(index: Int): T? {
        if (index < 0 || index >= count) return null
        if (index == 0) {
            val value = head?.value
            head = head?.next
            count--
            return value
        }
        var current = head
        for (i in 0 until index - 1) {
            current = current?.next
        }
        val value = current?.next?.value
        current?.next = current?.next?.next
        count--
        return value
    }

    fun get(index: Int): T? {
        if (index < 0 || index >= count) return null
        var current = head
        for (i in 0 until index) {
            current = current?.next
        }
        return current?.value
    }

    fun indexOf(value: T): Int {
        var current = head
        var index = 0
        while (current != null) {
            if (current.value == value) return index
            current = current.next
            index++
        }
        return -1
    }

    fun contains(value: T): Boolean = indexOf(value) != -1

    fun isEmpty(): Boolean = count == 0

    fun size(): Int = count

    fun clear() {
        head = null
        count = 0
    }

    fun toList(): List<T> {
        val result = mutableListOf<T>()
        var current = head
        while (current != null) {
            result.add(current.value)
            current = current.next
        }
        return result
    }
}
