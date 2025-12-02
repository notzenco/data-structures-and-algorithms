/// Binary search on a sorted slice.
/// Time: O(log n)
/// Space: O(1) iterative, O(log n) recursive

pub fn binary_search<T: Ord>(arr: &[T], target: &T) -> Option<usize> {
    if arr.is_empty() {
        return None;
    }

    let mut left = 0;
    let mut right = arr.len() - 1;

    while left <= right {
        let mid = left + (right - left) / 2;

        match arr[mid].cmp(target) {
            std::cmp::Ordering::Equal => return Some(mid),
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Greater => {
                if mid == 0 {
                    break;
                }
                right = mid - 1;
            }
        }
    }

    None
}

pub fn binary_search_recursive<T: Ord>(arr: &[T], target: &T) -> Option<usize> {
    if arr.is_empty() {
        return None;
    }
    search_recursive(arr, target, 0, arr.len() - 1)
}

fn search_recursive<T: Ord>(arr: &[T], target: &T, left: usize, right: usize) -> Option<usize> {
    if left > right {
        return None;
    }

    let mid = left + (right - left) / 2;

    match arr[mid].cmp(target) {
        std::cmp::Ordering::Equal => Some(mid),
        std::cmp::Ordering::Less => search_recursive(arr, target, mid + 1, right),
        std::cmp::Ordering::Greater => {
            if mid == 0 {
                None
            } else {
                search_recursive(arr, target, left, mid - 1)
            }
        }
    }
}

pub fn lower_bound<T: Ord>(arr: &[T], target: &T) -> usize {
    let mut left = 0;
    let mut right = arr.len();

    while left < right {
        let mid = left + (right - left) / 2;
        if arr[mid] < *target {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    left
}

pub fn upper_bound<T: Ord>(arr: &[T], target: &T) -> usize {
    let mut left = 0;
    let mut right = arr.len();

    while left < right {
        let mid = left + (right - left) / 2;
        if arr[mid] <= *target {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    left
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_array() {
        let arr: [i32; 0] = [];
        assert_eq!(binary_search(&arr, &5), None);
    }

    #[test]
    fn test_single_element() {
        let arr = [5];
        assert_eq!(binary_search(&arr, &5), Some(0));
        assert_eq!(binary_search(&arr, &3), None);
    }

    #[test]
    fn test_found() {
        let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        assert_eq!(binary_search(&arr, &1), Some(0));
        assert_eq!(binary_search(&arr, &5), Some(4));
        assert_eq!(binary_search(&arr, &10), Some(9));
    }

    #[test]
    fn test_not_found() {
        let arr = [1, 3, 5, 7, 9];
        assert_eq!(binary_search(&arr, &0), None);
        assert_eq!(binary_search(&arr, &2), None);
        assert_eq!(binary_search(&arr, &10), None);
    }

    #[test]
    fn test_recursive() {
        let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        assert_eq!(binary_search_recursive(&arr, &1), Some(0));
        assert_eq!(binary_search_recursive(&arr, &5), Some(4));
        assert_eq!(binary_search_recursive(&arr, &10), Some(9));
        assert_eq!(binary_search_recursive(&arr, &0), None);
    }

    #[test]
    fn test_lower_bound() {
        let arr = [1, 2, 4, 4, 4, 6, 7];
        assert_eq!(lower_bound(&arr, &4), 2);
        assert_eq!(lower_bound(&arr, &3), 2);
        assert_eq!(lower_bound(&arr, &0), 0);
        assert_eq!(lower_bound(&arr, &10), 7);
    }

    #[test]
    fn test_upper_bound() {
        let arr = [1, 2, 4, 4, 4, 6, 7];
        assert_eq!(upper_bound(&arr, &4), 5);
        assert_eq!(upper_bound(&arr, &3), 2);
        assert_eq!(upper_bound(&arr, &0), 0);
        assert_eq!(upper_bound(&arr, &7), 7);
    }

    #[test]
    fn test_strings() {
        let arr = ["apple", "banana", "cherry", "date"];
        assert_eq!(binary_search(&arr, &"banana"), Some(1));
        assert_eq!(binary_search(&arr, &"fig"), None);
    }
}
