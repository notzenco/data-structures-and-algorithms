package algorithms

import "cmp"

// InsertionSort sorts a slice in-place using insertion sort.
// Time: O(n^2) average, O(n) best (nearly sorted)
// Space: O(1)
func InsertionSort[T cmp.Ordered](arr []T) {
	for i := 1; i < len(arr); i++ {
		key := arr[i]
		j := i - 1

		for j >= 0 && arr[j] > key {
			arr[j+1] = arr[j]
			j--
		}
		arr[j+1] = key
	}
}

// InsertionSortFunc sorts a slice using a custom comparison function.
func InsertionSortFunc[T any](arr []T, less func(a, b T) bool) {
	for i := 1; i < len(arr); i++ {
		key := arr[i]
		j := i - 1

		for j >= 0 && less(key, arr[j]) {
			arr[j+1] = arr[j]
			j--
		}
		arr[j+1] = key
	}
}
