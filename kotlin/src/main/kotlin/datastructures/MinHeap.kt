package datastructures

/**
 * Binary min-heap implementation.
 * Time: O(log n) insert/extract, O(1) peek
 * Space: O(n)
 */
class MinHeap<T : Comparable<T>>(
    private val comparator: Comparator<T>? = null
) {
    private val heap = mutableListOf<T>()

    fun insert(value: T) {
        heap.add(value)
        siftUp(heap.lastIndex)
    }

    fun extractMin(): T? {
        if (heap.isEmpty()) return null

        val min = heap[0]
        val last = heap.removeAt(heap.lastIndex)

        if (heap.isNotEmpty()) {
            heap[0] = last
            siftDown(0)
        }

        return min
    }

    fun peek(): T? = heap.firstOrNull()

    fun isEmpty(): Boolean = heap.isEmpty()

    fun size(): Int = heap.size

    fun clear() {
        heap.clear()
    }

    fun toList(): List<T> = heap.toList()

    private fun siftUp(index: Int) {
        var i = index
        while (i > 0) {
            val parentIndex = (i - 1) / 2
            if (compare(heap[i], heap[parentIndex]) >= 0) break
            swap(i, parentIndex)
            i = parentIndex
        }
    }

    private fun siftDown(index: Int) {
        var i = index

        while (true) {
            val leftChild = 2 * i + 1
            val rightChild = 2 * i + 2
            var smallest = i

            if (leftChild < heap.size && compare(heap[leftChild], heap[smallest]) < 0) {
                smallest = leftChild
            }

            if (rightChild < heap.size && compare(heap[rightChild], heap[smallest]) < 0) {
                smallest = rightChild
            }

            if (smallest == i) break

            swap(i, smallest)
            i = smallest
        }
    }

    private fun swap(i: Int, j: Int) {
        val temp = heap[i]
        heap[i] = heap[j]
        heap[j] = temp
    }

    private fun compare(a: T, b: T): Int = comparator?.compare(a, b) ?: a.compareTo(b)
}
