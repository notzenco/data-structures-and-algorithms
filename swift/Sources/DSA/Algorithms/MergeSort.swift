/// Merge sort algorithm.
/// Time: O(n log n) all cases
/// Space: O(n)
/// Stable: Yes
public enum MergeSort {
    /// Sort an array in-place.
    public static func sort<T: Comparable>(_ array: inout [T], by comparator: ((T, T) -> Bool)? = nil) {
        array = mergeSort(array, by: comparator)
    }

    /// Return a sorted copy of the array.
    public static func sorted<T: Comparable>(_ array: [T], by comparator: ((T, T) -> Bool)? = nil) -> [T] {
        mergeSort(array, by: comparator)
    }

    private static func mergeSort<T: Comparable>(_ array: [T], by comparator: ((T, T) -> Bool)?) -> [T] {
        guard array.count > 1 else { return array }

        let compare = comparator ?? { $0 < $1 }
        let mid = array.count / 2
        let left = mergeSort(Array(array[..<mid]), by: comparator)
        let right = mergeSort(Array(array[mid...]), by: comparator)

        return merge(left, right, by: compare)
    }

    private static func merge<T>(_ left: [T], _ right: [T], by compare: (T, T) -> Bool) -> [T] {
        var result: [T] = []
        result.reserveCapacity(left.count + right.count)

        var i = 0
        var j = 0

        while i < left.count && j < right.count {
            if compare(left[i], right[j]) || (!compare(right[j], left[i]) && i < j) {
                result.append(left[i])
                i += 1
            } else {
                result.append(right[j])
                j += 1
            }
        }

        result.append(contentsOf: left[i...])
        result.append(contentsOf: right[j...])

        return result
    }
}
