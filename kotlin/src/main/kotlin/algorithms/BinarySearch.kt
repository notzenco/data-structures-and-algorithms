package algorithms

/**
 * Binary search algorithms.
 * Time: O(log n)
 * Space: O(1)
 */
object BinarySearch {
    /**
     * Find the index of target in a sorted list.
     * Returns -1 if not found.
     */
    fun <T : Comparable<T>> search(list: List<T>, target: T, comparator: Comparator<T>? = null): Int {
        var left = 0
        var right = list.lastIndex

        while (left <= right) {
            val mid = left + (right - left) / 2
            val cmp = compare(list[mid], target, comparator)

            when {
                cmp == 0 -> return mid
                cmp < 0 -> left = mid + 1
                else -> right = mid - 1
            }
        }

        return -1
    }

    /**
     * Find the leftmost index where target could be inserted to maintain sorted order.
     * Returns the index of the first element >= target.
     */
    fun <T : Comparable<T>> lowerBound(list: List<T>, target: T, comparator: Comparator<T>? = null): Int {
        var left = 0
        var right = list.size

        while (left < right) {
            val mid = left + (right - left) / 2
            if (compare(list[mid], target, comparator) < 0) {
                left = mid + 1
            } else {
                right = mid
            }
        }

        return left
    }

    /**
     * Find the rightmost index where target could be inserted to maintain sorted order.
     * Returns the index of the first element > target.
     */
    fun <T : Comparable<T>> upperBound(list: List<T>, target: T, comparator: Comparator<T>? = null): Int {
        var left = 0
        var right = list.size

        while (left < right) {
            val mid = left + (right - left) / 2
            if (compare(list[mid], target, comparator) <= 0) {
                left = mid + 1
            } else {
                right = mid
            }
        }

        return left
    }

    private fun <T : Comparable<T>> compare(a: T, b: T, comparator: Comparator<T>?): Int =
        comparator?.compare(a, b) ?: a.compareTo(b)
}
