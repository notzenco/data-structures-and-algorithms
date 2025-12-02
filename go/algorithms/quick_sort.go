package algorithms

import "cmp"

// QuickSort sorts a slice using quick sort with median-of-three pivot.
// Time: O(n log n) average, O(n^2) worst
// Space: O(log n) average
func QuickSort[T cmp.Ordered](arr []T) {
	if len(arr) < 2 {
		return
	}
	quickSort(arr, 0, len(arr)-1)
}

func quickSort[T cmp.Ordered](arr []T, low, high int) {
	if low >= high {
		return
	}

	pivotIdx := partition(arr, low, high)

	if pivotIdx > low {
		quickSort(arr, low, pivotIdx-1)
	}
	if pivotIdx < high {
		quickSort(arr, pivotIdx+1, high)
	}
}

func partition[T cmp.Ordered](arr []T, low, high int) int {
	// Median of three pivot selection
	mid := low + (high-low)/2

	if arr[high] < arr[low] {
		arr[low], arr[high] = arr[high], arr[low]
	}
	if arr[mid] < arr[low] {
		arr[low], arr[mid] = arr[mid], arr[low]
	}
	if arr[high] < arr[mid] {
		arr[mid], arr[high] = arr[high], arr[mid]
	}

	arr[mid], arr[high] = arr[high], arr[mid]
	pivot := arr[high]

	i := low
	for j := low; j < high; j++ {
		if arr[j] < pivot {
			arr[i], arr[j] = arr[j], arr[i]
			i++
		}
	}

	arr[i], arr[high] = arr[high], arr[i]
	return i
}

// QuickSortFunc sorts a slice using a custom comparison function.
func QuickSortFunc[T any](arr []T, less func(a, b T) bool) {
	if len(arr) < 2 {
		return
	}
	quickSortFunc(arr, 0, len(arr)-1, less)
}

func quickSortFunc[T any](arr []T, low, high int, less func(a, b T) bool) {
	if low >= high {
		return
	}

	pivotIdx := partitionFunc(arr, low, high, less)

	if pivotIdx > low {
		quickSortFunc(arr, low, pivotIdx-1, less)
	}
	if pivotIdx < high {
		quickSortFunc(arr, pivotIdx+1, high, less)
	}
}

func partitionFunc[T any](arr []T, low, high int, less func(a, b T) bool) int {
	mid := low + (high-low)/2

	if less(arr[high], arr[low]) {
		arr[low], arr[high] = arr[high], arr[low]
	}
	if less(arr[mid], arr[low]) {
		arr[low], arr[mid] = arr[mid], arr[low]
	}
	if less(arr[high], arr[mid]) {
		arr[mid], arr[high] = arr[high], arr[mid]
	}

	arr[mid], arr[high] = arr[high], arr[mid]
	pivot := arr[high]

	i := low
	for j := low; j < high; j++ {
		if less(arr[j], pivot) {
			arr[i], arr[j] = arr[j], arr[i]
			i++
		}
	}

	arr[i], arr[high] = arr[high], arr[i]
	return i
}
