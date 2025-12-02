/// Quick sort algorithm with median-of-three pivot selection.
/// Time: O(n log n) average, O(n²) worst case
/// Space: O(log n) average for recursion stack
/// Stable: No
public enum QuickSort {
    /// Sort an array in-place.
    public static func sort<T: Comparable>(_ array: inout [T], by comparator: ((T, T) -> Bool)? = nil) {
        let compare = comparator ?? { $0 < $1 }
        quickSort(&array, 0, array.count - 1, by: compare)
    }

    /// Return a sorted copy of the array.
    public static func sorted<T: Comparable>(_ array: [T], by comparator: ((T, T) -> Bool)? = nil) -> [T] {
        var result = array
        sort(&result, by: comparator)
        return result
    }

    private static func quickSort<T>(_ array: inout [T], _ low: Int, _ high: Int, by compare: (T, T) -> Bool) {
        guard low < high else { return }

        let pivotIndex = partition(&array, low, high, by: compare)
        quickSort(&array, low, pivotIndex - 1, by: compare)
        quickSort(&array, pivotIndex + 1, high, by: compare)
    }

    private static func partition<T>(_ array: inout [T], _ low: Int, _ high: Int, by compare: (T, T) -> Bool) -> Int {
        // Median-of-three pivot selection
        let mid = low + (high - low) / 2

        if compare(array[mid], array[low]) { array.swapAt(low, mid) }
        if compare(array[high], array[low]) { array.swapAt(low, high) }
        if compare(array[mid], array[high]) { array.swapAt(mid, high) }

        let pivot = array[high]
        var i = low - 1

        for j in low..<high {
            if compare(array[j], pivot) || (!compare(pivot, array[j])) {
                i += 1
                array.swapAt(i, j)
            }
        }

        array.swapAt(i + 1, high)
        return i + 1
    }
}
