package datastructures

/**
 * Doubly linked list with bidirectional traversal.
 * Time: O(1) for prepend/append/removeFirst/removeLast, O(n) for arbitrary access
 * Space: O(n)
 */
class DoublyLinkedList<T> {
    private class Node<T>(var value: T, var prev: Node<T>? = null, var next: Node<T>? = null)

    private var head: Node<T>? = null
    private var tail: Node<T>? = null
    private var count = 0

    fun prepend(value: T) {
        val node = Node(value, next = head)
        if (head != null) {
            head!!.prev = node
        } else {
            tail = node
        }
        head = node
        count++
    }

    fun append(value: T) {
        val node = Node(value, prev = tail)
        if (tail != null) {
            tail!!.next = node
        } else {
            head = node
        }
        tail = node
        count++
    }

    fun removeFirst(): T? {
        val node = head ?: return null
        head = node.next
        if (head != null) {
            head!!.prev = null
        } else {
            tail = null
        }
        count--
        return node.value
    }

    fun removeLast(): T? {
        val node = tail ?: return null
        tail = node.prev
        if (tail != null) {
            tail!!.next = null
        } else {
            head = null
        }
        count--
        return node.value
    }

    fun getFirst(): T? = head?.value

    fun getLast(): T? = tail?.value

    fun get(index: Int): T? {
        if (index < 0 || index >= count) return null
        val node = if (index < count / 2) {
            var current = head
            for (i in 0 until index) {
                current = current?.next
            }
            current
        } else {
            var current = tail
            for (i in count - 1 downTo index + 1) {
                current = current?.prev
            }
            current
        }
        return node?.value
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
        tail = null
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
