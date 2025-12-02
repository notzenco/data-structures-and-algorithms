package datastructures

/**
 * Resizable array with automatic capacity management.
 * Time: O(1) amortized for add, O(n) for insert/remove
 * Space: O(n)
 */
class DynamicArray<T> {
    private var items: Array<Any?> = arrayOfNulls(DEFAULT_CAPACITY)
    private var count = 0

    companion object {
        private const val DEFAULT_CAPACITY = 16
    }

    fun push(value: T) {
        ensureCapacity(count + 1)
        items[count++] = value
    }

    fun pop(): T? {
        if (count == 0) return null
        @Suppress("UNCHECKED_CAST")
        val value = items[--count] as T
        items[count] = null
        return value
    }

    @Suppress("UNCHECKED_CAST")
    fun get(index: Int): T? {
        if (index < 0 || index >= count) return null
        return items[index] as T
    }

    fun set(index: Int, value: T): Boolean {
        if (index < 0 || index >= count) return false
        items[index] = value
        return true
    }

    fun insert(index: Int, value: T): Boolean {
        if (index < 0 || index > count) return false
        ensureCapacity(count + 1)
        for (i in count downTo index + 1) {
            items[i] = items[i - 1]
        }
        items[index] = value
        count++
        return true
    }

    fun removeAt(index: Int): T? {
        if (index < 0 || index >= count) return null
        @Suppress("UNCHECKED_CAST")
        val value = items[index] as T
        for (i in index until count - 1) {
            items[i] = items[i + 1]
        }
        items[--count] = null
        return value
    }

    fun indexOf(value: T): Int {
        for (i in 0 until count) {
            if (items[i] == value) return i
        }
        return -1
    }

    fun contains(value: T): Boolean = indexOf(value) != -1

    fun isEmpty(): Boolean = count == 0

    fun size(): Int = count

    fun clear() {
        for (i in 0 until count) {
            items[i] = null
        }
        count = 0
    }

    @Suppress("UNCHECKED_CAST")
    fun toList(): List<T> = (0 until count).map { items[it] as T }

    private fun ensureCapacity(minCapacity: Int) {
        if (minCapacity > items.size) {
            val newCapacity = maxOf(items.size * 2, minCapacity)
            items = items.copyOf(newCapacity)
        }
    }
}
