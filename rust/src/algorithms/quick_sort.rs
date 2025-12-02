/// Quick sort implementation with median-of-three pivot selection.
/// Time: O(n log n) average, O(n^2) worst
/// Space: O(log n) average

pub fn quick_sort<T: Ord>(arr: &mut [T]) {
    let len = arr.len();
    if len < 2 {
        return;
    }
    quick_sort_impl(arr, 0, len - 1);
}

fn quick_sort_impl<T: Ord>(arr: &mut [T], low: usize, high: usize) {
    if low >= high {
        return;
    }

    let pivot_idx = partition(arr, low, high);

    if pivot_idx > low {
        quick_sort_impl(arr, low, pivot_idx - 1);
    }
    if pivot_idx < high {
        quick_sort_impl(arr, pivot_idx + 1, high);
    }
}

fn partition<T: Ord>(arr: &mut [T], low: usize, high: usize) -> usize {
    // Median of three pivot selection
    let mid = low + (high - low) / 2;

    if arr[high] < arr[low] {
        arr.swap(low, high);
    }
    if arr[mid] < arr[low] {
        arr.swap(low, mid);
    }
    if arr[high] < arr[mid] {
        arr.swap(mid, high);
    }

    arr.swap(mid, high);

    let mut i = low;
    for j in low..high {
        if arr[j] < arr[high] {
            arr.swap(i, j);
            i += 1;
        }
    }
    arr.swap(i, high);
    i
}

pub fn quick_sort_by<T, F>(arr: &mut [T], compare: &F)
where
    F: Fn(&T, &T) -> std::cmp::Ordering,
{
    let len = arr.len();
    if len < 2 {
        return;
    }
    quick_sort_by_impl(arr, 0, len - 1, compare);
}

fn quick_sort_by_impl<T, F>(arr: &mut [T], low: usize, high: usize, compare: &F)
where
    F: Fn(&T, &T) -> std::cmp::Ordering,
{
    if low >= high {
        return;
    }

    let pivot_idx = partition_by(arr, low, high, compare);

    if pivot_idx > low {
        quick_sort_by_impl(arr, low, pivot_idx - 1, compare);
    }
    if pivot_idx < high {
        quick_sort_by_impl(arr, pivot_idx + 1, high, compare);
    }
}

fn partition_by<T, F>(arr: &mut [T], low: usize, high: usize, compare: &F) -> usize
where
    F: Fn(&T, &T) -> std::cmp::Ordering,
{
    let mid = low + (high - low) / 2;

    if compare(&arr[high], &arr[low]) == std::cmp::Ordering::Less {
        arr.swap(low, high);
    }
    if compare(&arr[mid], &arr[low]) == std::cmp::Ordering::Less {
        arr.swap(low, mid);
    }
    if compare(&arr[high], &arr[mid]) == std::cmp::Ordering::Less {
        arr.swap(mid, high);
    }

    arr.swap(mid, high);

    let mut i = low;
    for j in low..high {
        if compare(&arr[j], &arr[high]) == std::cmp::Ordering::Less {
            arr.swap(i, j);
            i += 1;
        }
    }
    arr.swap(i, high);
    i
}

pub fn quick_sort_desc<T: Ord>(arr: &mut [T]) {
    quick_sort_by(arr, &|a, b| b.cmp(a));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_array() {
        let mut arr: [i32; 0] = [];
        quick_sort(&mut arr);
        assert!(arr.is_empty());
    }

    #[test]
    fn test_single_element() {
        let mut arr = [42];
        quick_sort(&mut arr);
        assert_eq!(arr[0], 42);
    }

    #[test]
    fn test_two_elements() {
        let mut arr = [2, 1];
        quick_sort(&mut arr);
        assert_eq!(arr, [1, 2]);
    }

    #[test]
    fn test_sorted_array() {
        let mut arr = [1, 2, 3, 4, 5];
        quick_sort(&mut arr);
        assert_eq!(arr, [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_reverse_sorted() {
        let mut arr = [5, 4, 3, 2, 1];
        quick_sort(&mut arr);
        assert_eq!(arr, [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_random_array() {
        let mut arr = [38, 27, 43, 3, 9, 82, 10];
        quick_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_duplicates() {
        let mut arr = [5, 2, 5, 3, 5, 1, 5];
        quick_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_all_same() {
        let mut arr = [7, 7, 7, 7, 7];
        quick_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_negative_numbers() {
        let mut arr = [-5, 3, -1, 0, -10, 7];
        quick_sort(&mut arr);
        assert_eq!(arr[0], -10);
    }

    #[test]
    fn test_descending() {
        let mut arr = [1, 5, 3, 2, 4];
        quick_sort_desc(&mut arr);
        assert_eq!(arr[0], 5);
        assert!(arr.windows(2).all(|w| w[0] >= w[1]));
    }

    #[test]
    fn test_large_array() {
        let mut arr: Vec<i32> = (0..1000).rev().collect();
        quick_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_strings() {
        let mut arr = ["banana", "apple", "cherry", "date"];
        quick_sort(&mut arr);
        assert_eq!(arr[0], "apple");
    }
}
