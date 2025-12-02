package datastructures

/**
 * Double-ended queue supporting operations at both ends.
 * Time: O(1) for all operations
 * Space: O(n)
 */
class Deque<T> {
    private class Node<T>(val value: T, var prev: Node<T>? = null, var next: Node<T>? = null)

    private var head: Node<T>? = null
    private var tail: Node<T>? = null
    private var count = 0

    fun pushFront(value: T) {
        val node = Node(value, next = head)
        if (head != null) {
            head!!.prev = node
        } else {
            tail = node
        }
        head = node
        count++
    }

    fun pushBack(value: T) {
        val node = Node(value, prev = tail)
        if (tail != null) {
            tail!!.next = node
        } else {
            head = node
        }
        tail = node
        count++
    }

    fun popFront(): T? {
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

    fun popBack(): T? {
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

    fun peekFront(): T? = head?.value

    fun peekBack(): T? = tail?.value

    fun isEmpty(): Boolean = count == 0

    fun size(): Int = count

    fun clear() {
        head = null
        tail = null
        count = 0
    }
}
