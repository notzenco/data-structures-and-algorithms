const std = @import("std");

/// Quick sort with median-of-three pivot selection.
/// Time: O(n log n) average, O(n²) worst case
/// Space: O(log n) stack space

pub fn sort(comptime T: type, arr: []T) void {
    if (arr.len <= 1) return;
    quickSort(T, arr, 0, arr.len - 1);
}

fn quickSort(comptime T: type, arr: []T, low: usize, high: usize) void {
    if (low >= high) return;

    const pivot_index = partition(T, arr, low, high);

    if (pivot_index > 0) {
        quickSort(T, arr, low, pivot_index - 1);
    }
    quickSort(T, arr, pivot_index + 1, high);
}

fn partition(comptime T: type, arr: []T, low: usize, high: usize) usize {
    const pivot_index = medianOfThree(T, arr, low, high);
    const temp = arr[pivot_index];
    arr[pivot_index] = arr[high];
    arr[high] = temp;

    const pivot = arr[high];
    var i = low;

    for (low..high) |j| {
        if (arr[j] <= pivot) {
            const t = arr[i];
            arr[i] = arr[j];
            arr[j] = t;
            i += 1;
        }
    }

    const t = arr[i];
    arr[i] = arr[high];
    arr[high] = t;

    return i;
}

fn medianOfThree(comptime T: type, arr: []T, low: usize, high: usize) usize {
    const mid = low + (high - low) / 2;

    if (arr[low] > arr[mid]) {
        const t = arr[low];
        arr[low] = arr[mid];
        arr[mid] = t;
    }
    if (arr[low] > arr[high]) {
        const t = arr[low];
        arr[low] = arr[high];
        arr[high] = t;
    }
    if (arr[mid] > arr[high]) {
        const t = arr[mid];
        arr[mid] = arr[high];
        arr[high] = t;
    }

    return mid;
}

test "quick sort" {
    var arr = [_]i32{ 5, 2, 8, 1, 9, 3 };
    sort(i32, &arr);
    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3, 5, 8, 9 }, &arr);
}
