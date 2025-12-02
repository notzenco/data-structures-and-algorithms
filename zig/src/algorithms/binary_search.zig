const std = @import("std");

/// Binary search algorithms for sorted slices.
/// Time: O(log n)
/// Space: O(1)

pub fn search(comptime T: type, arr: []const T, target: T) ?usize {
    if (arr.len == 0) return null;

    var left: usize = 0;
    var right: usize = arr.len - 1;

    while (left <= right) {
        const mid = left + (right - left) / 2;

        if (arr[mid] == target) return mid;
        if (arr[mid] < target) {
            left = mid + 1;
        } else {
            if (mid == 0) break;
            right = mid - 1;
        }
    }

    return null;
}

pub fn lowerBound(comptime T: type, arr: []const T, target: T) usize {
    var left: usize = 0;
    var right: usize = arr.len;

    while (left < right) {
        const mid = left + (right - left) / 2;
        if (arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    return left;
}

pub fn upperBound(comptime T: type, arr: []const T, target: T) usize {
    var left: usize = 0;
    var right: usize = arr.len;

    while (left < right) {
        const mid = left + (right - left) / 2;
        if (arr[mid] <= target) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    return left;
}

pub fn contains(comptime T: type, arr: []const T, target: T) bool {
    return search(T, arr, target) != null;
}

test "binary search" {
    const arr = [_]i32{ 1, 3, 5, 7, 9, 11, 13 };
    try std.testing.expectEqual(@as(?usize, 3), search(i32, &arr, 7));
    try std.testing.expectEqual(@as(?usize, 0), search(i32, &arr, 1));
    try std.testing.expectEqual(@as(?usize, null), search(i32, &arr, 6));
}
