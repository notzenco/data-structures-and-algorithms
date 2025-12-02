package algorithms

/**
 * Merge sort algorithm.
 * Time: O(n log n) all cases
 * Space: O(n)
 * Stable: Yes
 */
object MergeSort {
    /**
     * Sort a mutable list in-place.
     */
    fun <T : Comparable<T>> sort(list: MutableList<T>, comparator: Comparator<T>? = null) {
        val sorted = mergeSort(list, comparator)
        for (i in sorted.indices) {
            list[i] = sorted[i]
        }
    }

    /**
     * Return a sorted copy of the list.
     */
    fun <T : Comparable<T>> sorted(list: List<T>, comparator: Comparator<T>? = null): List<T> =
        mergeSort(list, comparator)

    private fun <T : Comparable<T>> mergeSort(list: List<T>, comparator: Comparator<T>?): List<T> {
        if (list.size <= 1) return list

        val mid = list.size / 2
        val left = mergeSort(list.subList(0, mid), comparator)
        val right = mergeSort(list.subList(mid, list.size), comparator)

        return merge(left, right, comparator)
    }

    private fun <T : Comparable<T>> merge(left: List<T>, right: List<T>, comparator: Comparator<T>?): List<T> {
        val result = mutableListOf<T>()
        var i = 0
        var j = 0

        while (i < left.size && j < right.size) {
            if (compare(left[i], right[j], comparator) <= 0) {
                result.add(left[i++])
            } else {
                result.add(right[j++])
            }
        }

        while (i < left.size) result.add(left[i++])
        while (j < right.size) result.add(right[j++])

        return result
    }

    private fun <T : Comparable<T>> compare(a: T, b: T, comparator: Comparator<T>?): Int =
        comparator?.compare(a, b) ?: a.compareTo(b)
}
