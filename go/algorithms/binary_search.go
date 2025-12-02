package algorithms

import "cmp"

// BinarySearch searches for target in a sorted slice.
// Returns the index if found, -1 otherwise.
// Time: O(log n)
// Space: O(1)
func BinarySearch[T cmp.Ordered](arr []T, target T) int {
	left, right := 0, len(arr)-1

	for left <= right {
		mid := left + (right-left)/2

		if arr[mid] == target {
			return mid
		} else if arr[mid] < target {
			left = mid + 1
		} else {
			right = mid - 1
		}
	}

	return -1
}

// LowerBound returns the index of the first element not less than target.
func LowerBound[T cmp.Ordered](arr []T, target T) int {
	left, right := 0, len(arr)

	for left < right {
		mid := left + (right-left)/2
		if arr[mid] < target {
			left = mid + 1
		} else {
			right = mid
		}
	}

	return left
}

// UpperBound returns the index of the first element greater than target.
func UpperBound[T cmp.Ordered](arr []T, target T) int {
	left, right := 0, len(arr)

	for left < right {
		mid := left + (right-left)/2
		if arr[mid] <= target {
			left = mid + 1
		} else {
			right = mid
		}
	}

	return left
}
