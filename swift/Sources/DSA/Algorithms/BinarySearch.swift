/// Binary search algorithms.
/// Time: O(log n)
/// Space: O(1)
public enum BinarySearch {
    /// Find the index of target in a sorted array.
    /// Returns -1 if not found.
    public static func search<T: Comparable>(_ array: [T], _ target: T) -> Int {
        var left = 0
        var right = array.count - 1

        while left <= right {
            let mid = left + (right - left) / 2

            if array[mid] == target {
                return mid
            } else if array[mid] < target {
                left = mid + 1
            } else {
                right = mid - 1
            }
        }

        return -1
    }

    /// Find the leftmost index where target could be inserted to maintain sorted order.
    /// Returns the index of the first element >= target.
    public static func lowerBound<T: Comparable>(_ array: [T], _ target: T) -> Int {
        var left = 0
        var right = array.count

        while left < right {
            let mid = left + (right - left) / 2
            if array[mid] < target {
                left = mid + 1
            } else {
                right = mid
            }
        }

        return left
    }

    /// Find the rightmost index where target could be inserted to maintain sorted order.
    /// Returns the index of the first element > target.
    public static func upperBound<T: Comparable>(_ array: [T], _ target: T) -> Int {
        var left = 0
        var right = array.count

        while left < right {
            let mid = left + (right - left) / 2
            if array[mid] <= target {
                left = mid + 1
            } else {
                right = mid
            }
        }

        return left
    }
}
