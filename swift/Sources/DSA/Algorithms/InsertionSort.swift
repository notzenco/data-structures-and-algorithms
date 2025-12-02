/// Insertion sort algorithm.
/// Time: O(n²) worst/average, O(n) best (already sorted)
/// Space: O(1)
/// Stable: Yes
public enum InsertionSort {
    /// Sort an array in-place.
    public static func sort<T: Comparable>(_ array: inout [T], by comparator: ((T, T) -> Bool)? = nil) {
        let compare = comparator ?? { $0 < $1 }

        for i in 1..<array.count {
            let key = array[i]
            var j = i - 1

            while j >= 0 && compare(key, array[j]) {
                array[j + 1] = array[j]
                j -= 1
            }
            array[j + 1] = key
        }
    }

    /// Return a sorted copy of the array.
    public static func sorted<T: Comparable>(_ array: [T], by comparator: ((T, T) -> Bool)? = nil) -> [T] {
        var result = array
        sort(&result, by: comparator)
        return result
    }
}
