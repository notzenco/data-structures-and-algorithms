const std = @import("std");
const Allocator = std.mem.Allocator;

/// Merge sort - stable, divide-and-conquer sorting algorithm.
/// Time: O(n log n)
/// Space: O(n)

pub fn sort(comptime T: type, arr: []T, allocator: Allocator) !void {
    if (arr.len <= 1) return;

    const temp = try allocator.alloc(T, arr.len);
    defer allocator.free(temp);

    try mergeSort(T, arr, temp, 0, arr.len - 1);
}

fn mergeSort(comptime T: type, arr: []T, temp: []T, left: usize, right: usize) !void {
    if (left >= right) return;

    const mid = left + (right - left) / 2;
    try mergeSort(T, arr, temp, left, mid);
    try mergeSort(T, arr, temp, mid + 1, right);
    merge(T, arr, temp, left, mid, right);
}

fn merge(comptime T: type, arr: []T, temp: []T, left: usize, mid: usize, right: usize) void {
    @memcpy(temp[left .. right + 1], arr[left .. right + 1]);

    var i = left;
    var j = mid + 1;
    var k = left;

    while (i <= mid and j <= right) {
        if (temp[i] <= temp[j]) {
            arr[k] = temp[i];
            i += 1;
        } else {
            arr[k] = temp[j];
            j += 1;
        }
        k += 1;
    }

    while (i <= mid) {
        arr[k] = temp[i];
        i += 1;
        k += 1;
    }

    while (j <= right) {
        arr[k] = temp[j];
        j += 1;
        k += 1;
    }
}

test "merge sort" {
    const allocator = std.testing.allocator;
    var arr = [_]i32{ 5, 2, 8, 1, 9, 3 };
    try sort(i32, &arr, allocator);
    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3, 5, 8, 9 }, &arr);
}
