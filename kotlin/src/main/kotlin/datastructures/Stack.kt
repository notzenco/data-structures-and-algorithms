package datastructures

/**
 * LIFO (Last In, First Out) data structure.
 * Time: O(1) for all operations
 * Space: O(n)
 */
class Stack<T> {
    private val items = mutableListOf<T>()

    fun push(value: T) {
        items.add(value)
    }

    fun pop(): T? = if (items.isEmpty()) null else items.removeAt(items.lastIndex)

    fun peek(): T? = items.lastOrNull()

    fun isEmpty(): Boolean = items.isEmpty()

    fun size(): Int = items.size

    fun clear() {
        items.clear()
    }

    fun toList(): List<T> = items.toList()
}
