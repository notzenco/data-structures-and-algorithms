const std = @import("std");

/// Insertion sort - stable, in-place sorting algorithm.
/// Time: O(n²)
/// Space: O(1)

pub fn sort(comptime T: type, arr: []T) void {
    if (arr.len <= 1) return;

    for (1..arr.len) |i| {
        const key = arr[i];
        var j: usize = i;

        while (j > 0 and arr[j - 1] > key) {
            arr[j] = arr[j - 1];
            j -= 1;
        }

        arr[j] = key;
    }
}

pub fn sortBy(comptime T: type, arr: []T, lessThan: fn (T, T) bool) void {
    if (arr.len <= 1) return;

    for (1..arr.len) |i| {
        const key = arr[i];
        var j: usize = i;

        while (j > 0 and lessThan(key, arr[j - 1])) {
            arr[j] = arr[j - 1];
            j -= 1;
        }

        arr[j] = key;
    }
}

test "insertion sort" {
    var arr = [_]i32{ 5, 2, 8, 1, 9, 3 };
    sort(i32, &arr);
    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3, 5, 8, 9 }, &arr);
}
