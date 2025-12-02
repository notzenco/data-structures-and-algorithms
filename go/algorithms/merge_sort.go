package algorithms

import "cmp"

// MergeSort sorts a slice using merge sort.
// Time: O(n log n) all cases
// Space: O(n)
func MergeSort[T cmp.Ordered](arr []T) {
	if len(arr) < 2 {
		return
	}
	mergeSort(arr, 0, len(arr)-1)
}

func mergeSort[T cmp.Ordered](arr []T, left, right int) {
	if left >= right {
		return
	}

	mid := left + (right-left)/2
	mergeSort(arr, left, mid)
	mergeSort(arr, mid+1, right)
	merge(arr, left, mid, right)
}

func merge[T cmp.Ordered](arr []T, left, mid, right int) {
	leftArr := make([]T, mid-left+1)
	rightArr := make([]T, right-mid)

	copy(leftArr, arr[left:mid+1])
	copy(rightArr, arr[mid+1:right+1])

	i, j, k := 0, 0, left

	for i < len(leftArr) && j < len(rightArr) {
		if leftArr[i] <= rightArr[j] {
			arr[k] = leftArr[i]
			i++
		} else {
			arr[k] = rightArr[j]
			j++
		}
		k++
	}

	for i < len(leftArr) {
		arr[k] = leftArr[i]
		i++
		k++
	}

	for j < len(rightArr) {
		arr[k] = rightArr[j]
		j++
		k++
	}
}

// MergeSortFunc sorts a slice using a custom comparison function.
func MergeSortFunc[T any](arr []T, less func(a, b T) bool) {
	if len(arr) < 2 {
		return
	}
	mergeSortFunc(arr, 0, len(arr)-1, less)
}

func mergeSortFunc[T any](arr []T, left, right int, less func(a, b T) bool) {
	if left >= right {
		return
	}

	mid := left + (right-left)/2
	mergeSortFunc(arr, left, mid, less)
	mergeSortFunc(arr, mid+1, right, less)
	mergeFunc(arr, left, mid, right, less)
}

func mergeFunc[T any](arr []T, left, mid, right int, less func(a, b T) bool) {
	leftArr := make([]T, mid-left+1)
	rightArr := make([]T, right-mid)

	copy(leftArr, arr[left:mid+1])
	copy(rightArr, arr[mid+1:right+1])

	i, j, k := 0, 0, left

	for i < len(leftArr) && j < len(rightArr) {
		if !less(rightArr[j], leftArr[i]) {
			arr[k] = leftArr[i]
			i++
		} else {
			arr[k] = rightArr[j]
			j++
		}
		k++
	}

	for i < len(leftArr) {
		arr[k] = leftArr[i]
		i++
		k++
	}

	for j < len(rightArr) {
		arr[k] = rightArr[j]
		j++
		k++
	}
}
