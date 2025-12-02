/// Merge sort implementation.
/// Time: O(n log n) all cases
/// Space: O(n)

pub fn merge_sort<T: Ord + Clone>(arr: &mut [T]) {
    let len = arr.len();
    if len < 2 {
        return;
    }

    let mid = len / 2;
    merge_sort(&mut arr[..mid]);
    merge_sort(&mut arr[mid..]);

    let mut merged = Vec::with_capacity(len);
    let (left, right) = arr.split_at(mid);

    let mut i = 0;
    let mut j = 0;

    while i < left.len() && j < right.len() {
        if left[i] <= right[j] {
            merged.push(left[i].clone());
            i += 1;
        } else {
            merged.push(right[j].clone());
            j += 1;
        }
    }

    merged.extend_from_slice(&left[i..]);
    merged.extend_from_slice(&right[j..]);

    arr.clone_from_slice(&merged);
}

pub fn merge_sort_by<T: Clone, F>(arr: &mut [T], compare: &F)
where
    F: Fn(&T, &T) -> std::cmp::Ordering,
{
    let len = arr.len();
    if len < 2 {
        return;
    }

    let mid = len / 2;
    merge_sort_by(&mut arr[..mid], compare);
    merge_sort_by(&mut arr[mid..], compare);

    let mut merged = Vec::with_capacity(len);
    let (left, right) = arr.split_at(mid);

    let mut i = 0;
    let mut j = 0;

    while i < left.len() && j < right.len() {
        if compare(&left[i], &right[j]) != std::cmp::Ordering::Greater {
            merged.push(left[i].clone());
            i += 1;
        } else {
            merged.push(right[j].clone());
            j += 1;
        }
    }

    merged.extend_from_slice(&left[i..]);
    merged.extend_from_slice(&right[j..]);

    arr.clone_from_slice(&merged);
}

pub fn merge_sort_desc<T: Ord + Clone>(arr: &mut [T]) {
    merge_sort_by(arr, &|a, b| b.cmp(a));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_array() {
        let mut arr: [i32; 0] = [];
        merge_sort(&mut arr);
        assert!(arr.is_empty());
    }

    #[test]
    fn test_single_element() {
        let mut arr = [42];
        merge_sort(&mut arr);
        assert_eq!(arr[0], 42);
    }

    #[test]
    fn test_two_elements() {
        let mut arr = [2, 1];
        merge_sort(&mut arr);
        assert_eq!(arr, [1, 2]);
    }

    #[test]
    fn test_sorted_array() {
        let mut arr = [1, 2, 3, 4, 5];
        merge_sort(&mut arr);
        assert_eq!(arr, [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_reverse_sorted() {
        let mut arr = [5, 4, 3, 2, 1];
        merge_sort(&mut arr);
        assert_eq!(arr, [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_random_array() {
        let mut arr = [38, 27, 43, 3, 9, 82, 10];
        merge_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_duplicates() {
        let mut arr = [5, 2, 5, 3, 5, 1, 5];
        merge_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_negative_numbers() {
        let mut arr = [-5, 3, -1, 0, -10, 7];
        merge_sort(&mut arr);
        assert_eq!(arr[0], -10);
    }

    #[test]
    fn test_descending() {
        let mut arr = [1, 5, 3, 2, 4];
        merge_sort_desc(&mut arr);
        assert_eq!(arr[0], 5);
        assert!(arr.windows(2).all(|w| w[0] >= w[1]));
    }

    #[test]
    fn test_large_array() {
        let mut arr: Vec<i32> = (0..1000).rev().collect();
        merge_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_stability() {
        let mut arr = [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd')];
        merge_sort_by(&mut arr, &|a, b| a.0.cmp(&b.0));
        assert_eq!(arr[0].1, 'a');
        assert_eq!(arr[1].1, 'c');
    }
}
