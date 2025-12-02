package algorithms

/**
 * Insertion sort algorithm.
 * Time: O(n^2) worst/average, O(n) best (already sorted)
 * Space: O(1)
 * Stable: Yes
 */
object InsertionSort {
    /**
     * Sort a mutable list in-place.
     */
    fun <T : Comparable<T>> sort(list: MutableList<T>, comparator: Comparator<T>? = null) {
        for (i in 1 until list.size) {
            val key = list[i]
            var j = i - 1

            while (j >= 0 && compare(list[j], key, comparator) > 0) {
                list[j + 1] = list[j]
                j--
            }
            list[j + 1] = key
        }
    }

    /**
     * Return a sorted copy of the list.
     */
    fun <T : Comparable<T>> sorted(list: List<T>, comparator: Comparator<T>? = null): List<T> {
        val result = list.toMutableList()
        sort(result, comparator)
        return result
    }

    private fun <T : Comparable<T>> compare(a: T, b: T, comparator: Comparator<T>?): Int =
        comparator?.compare(a, b) ?: a.compareTo(b)
}
