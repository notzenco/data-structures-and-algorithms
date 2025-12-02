/// Insertion sort implementation.
/// Time: O(n^2) worst/average, O(n) best
/// Space: O(1)

pub fn insertion_sort<T: Ord>(arr: &mut [T]) {
    for i in 1..arr.len() {
        let mut j = i;
        while j > 0 && arr[j - 1] > arr[j] {
            arr.swap(j - 1, j);
            j -= 1;
        }
    }
}

pub fn insertion_sort_by<T, F>(arr: &mut [T], compare: F)
where
    F: Fn(&T, &T) -> std::cmp::Ordering,
{
    for i in 1..arr.len() {
        let mut j = i;
        while j > 0 && compare(&arr[j - 1], &arr[j]) == std::cmp::Ordering::Greater {
            arr.swap(j - 1, j);
            j -= 1;
        }
    }
}

pub fn insertion_sort_desc<T: Ord>(arr: &mut [T]) {
    insertion_sort_by(arr, |a, b| b.cmp(a));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_array() {
        let mut arr: [i32; 0] = [];
        insertion_sort(&mut arr);
        assert!(arr.is_empty());
    }

    #[test]
    fn test_single_element() {
        let mut arr = [42];
        insertion_sort(&mut arr);
        assert_eq!(arr[0], 42);
    }

    #[test]
    fn test_sorted_array() {
        let mut arr = [1, 2, 3, 4, 5];
        insertion_sort(&mut arr);
        assert_eq!(arr, [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_reverse_sorted() {
        let mut arr = [5, 4, 3, 2, 1];
        insertion_sort(&mut arr);
        assert_eq!(arr, [1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_random_array() {
        let mut arr = [38, 27, 43, 3, 9, 82, 10];
        insertion_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_duplicates() {
        let mut arr = [5, 2, 5, 3, 5, 1, 5];
        insertion_sort(&mut arr);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_negative_numbers() {
        let mut arr = [-5, 3, -1, 0, -10, 7];
        insertion_sort(&mut arr);
        assert_eq!(arr[0], -10);
        assert!(arr.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn test_descending() {
        let mut arr = [1, 5, 3, 2, 4];
        insertion_sort_desc(&mut arr);
        assert_eq!(arr[0], 5);
        assert!(arr.windows(2).all(|w| w[0] >= w[1]));
    }

    #[test]
    fn test_strings() {
        let mut arr = ["banana", "apple", "cherry", "date"];
        insertion_sort(&mut arr);
        assert_eq!(arr[0], "apple");
    }

    #[test]
    fn test_stability() {
        let mut arr = [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd')];
        insertion_sort_by(&mut arr, |a, b| a.0.cmp(&b.0));
        assert_eq!(arr[0].1, 'a');
        assert_eq!(arr[1].1, 'c');
    }
}
