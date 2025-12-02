package algorithms

/**
 * Quick sort algorithm with median-of-three pivot selection.
 * Time: O(n log n) average, O(n^2) worst case
 * Space: O(log n) average for recursion stack
 * Stable: No
 */
object QuickSort {
    /**
     * Sort a mutable list in-place.
     */
    fun <T : Comparable<T>> sort(list: MutableList<T>, comparator: Comparator<T>? = null) {
        quickSort(list, 0, list.lastIndex, comparator)
    }

    /**
     * Return a sorted copy of the list.
     */
    fun <T : Comparable<T>> sorted(list: List<T>, comparator: Comparator<T>? = null): List<T> {
        val result = list.toMutableList()
        sort(result, comparator)
        return result
    }

    private fun <T : Comparable<T>> quickSort(
        list: MutableList<T>,
        low: Int,
        high: Int,
        comparator: Comparator<T>?
    ) {
        if (low < high) {
            val pivotIndex = partition(list, low, high, comparator)
            quickSort(list, low, pivotIndex - 1, comparator)
            quickSort(list, pivotIndex + 1, high, comparator)
        }
    }

    private fun <T : Comparable<T>> partition(
        list: MutableList<T>,
        low: Int,
        high: Int,
        comparator: Comparator<T>?
    ): Int {
        // Median-of-three pivot selection
        val mid = low + (high - low) / 2

        if (compare(list[mid], list[low], comparator) < 0) swap(list, low, mid)
        if (compare(list[high], list[low], comparator) < 0) swap(list, low, high)
        if (compare(list[mid], list[high], comparator) < 0) swap(list, mid, high)

        val pivot = list[high]
        var i = low - 1

        for (j in low until high) {
            if (compare(list[j], pivot, comparator) <= 0) {
                i++
                swap(list, i, j)
            }
        }

        swap(list, i + 1, high)
        return i + 1
    }

    private fun <T> swap(list: MutableList<T>, i: Int, j: Int) {
        val temp = list[i]
        list[i] = list[j]
        list[j] = temp
    }

    private fun <T : Comparable<T>> compare(a: T, b: T, comparator: Comparator<T>?): Int =
        comparator?.compare(a, b) ?: a.compareTo(b)
}
