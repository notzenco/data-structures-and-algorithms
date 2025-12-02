package datastructures

/**
 * Hash table implementation with open addressing and linear probing.
 * Time: O(1) average for all operations
 * Space: O(n)
 */
class HashTable<K, V> {
    private data class Entry<K, V>(val key: K, var value: V, var deleted: Boolean = false)

    private var buckets: Array<Entry<K, V>?> = arrayOfNulls(DEFAULT_CAPACITY)
    private var count = 0

    companion object {
        private const val DEFAULT_CAPACITY = 16
        private const val LOAD_FACTOR = 0.7
    }

    fun put(key: K, value: V) {
        if (count >= buckets.size * LOAD_FACTOR) {
            resize(buckets.size * 2)
        }

        var index = hash(key)
        var firstDeleted = -1

        for (i in buckets.indices) {
            val entry = buckets[index]

            when {
                entry == null -> {
                    val insertIndex = if (firstDeleted != -1) firstDeleted else index
                    buckets[insertIndex] = Entry(key, value)
                    count++
                    return
                }
                entry.deleted && firstDeleted == -1 -> {
                    firstDeleted = index
                }
                !entry.deleted && entry.key == key -> {
                    entry.value = value
                    return
                }
            }

            index = (index + 1) % buckets.size
        }

        if (firstDeleted != -1) {
            buckets[firstDeleted] = Entry(key, value)
            count++
        }
    }

    fun get(key: K): V? {
        val index = findIndex(key)
        return if (index != -1) buckets[index]?.value else null
    }

    fun remove(key: K): V? {
        val index = findIndex(key)
        if (index == -1) return null
        val entry = buckets[index]!!
        val value = entry.value
        entry.deleted = true
        count--
        return value
    }

    fun contains(key: K): Boolean = findIndex(key) != -1

    fun isEmpty(): Boolean = count == 0

    fun size(): Int = count

    fun keys(): List<K> = buckets.filterNotNull().filter { !it.deleted }.map { it.key }

    fun values(): List<V> = buckets.filterNotNull().filter { !it.deleted }.map { it.value }

    fun clear() {
        buckets = arrayOfNulls(DEFAULT_CAPACITY)
        count = 0
    }

    private fun hash(key: K): Int = (key.hashCode() and Int.MAX_VALUE) % buckets.size

    private fun findIndex(key: K): Int {
        var index = hash(key)

        for (i in buckets.indices) {
            val entry = buckets[index] ?: return -1
            if (!entry.deleted && entry.key == key) return index
            index = (index + 1) % buckets.size
        }

        return -1
    }

    private fun resize(newCapacity: Int) {
        val oldBuckets = buckets
        buckets = arrayOfNulls(newCapacity)
        count = 0

        for (entry in oldBuckets) {
            if (entry != null && !entry.deleted) {
                put(entry.key, entry.value)
            }
        }
    }
}
