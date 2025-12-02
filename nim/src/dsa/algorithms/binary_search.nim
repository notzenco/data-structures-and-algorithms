## Binary Search - Search in sorted arrays
##
## Time Complexity: O(log n)
## Space Complexity: O(1)

proc binarySearch*[T](arr: openArray[T], target: T): int =
  ## Search for target in sorted array
  ## Returns index of target or -1 if not found
  var left = 0
  var right = arr.len - 1

  while left <= right:
    let mid = left + (right - left) div 2
    if arr[mid] == target:
      return mid
    elif arr[mid] < target:
      left = mid + 1
    else:
      right = mid - 1

  -1

proc binarySearchRecursive*[T](arr: openArray[T], target: T): int =
  ## Recursive binary search
  proc search(left, right: int): int =
    if left > right:
      return -1
    let mid = left + (right - left) div 2
    if arr[mid] == target:
      return mid
    elif arr[mid] < target:
      return search(mid + 1, right)
    else:
      return search(left, mid - 1)

  search(0, arr.len - 1)

proc lowerBound*[T](arr: openArray[T], target: T): int =
  ## Find the first position where target could be inserted
  ## Returns the index of the first element >= target
  var left = 0
  var right = arr.len

  while left < right:
    let mid = left + (right - left) div 2
    if arr[mid] < target:
      left = mid + 1
    else:
      right = mid

  left

proc upperBound*[T](arr: openArray[T], target: T): int =
  ## Find the last position where target could be inserted
  ## Returns the index of the first element > target
  var left = 0
  var right = arr.len

  while left < right:
    let mid = left + (right - left) div 2
    if arr[mid] <= target:
      left = mid + 1
    else:
      right = mid

  left

proc equalRange*[T](arr: openArray[T], target: T): (int, int) =
  ## Find the range of elements equal to target
  ## Returns (lower_bound, upper_bound)
  (lowerBound(arr, target), upperBound(arr, target))

proc contains*[T](arr: openArray[T], target: T): bool =
  ## Check if sorted array contains target
  binarySearch(arr, target) != -1

proc countEqual*[T](arr: openArray[T], target: T): int =
  ## Count occurrences of target in sorted array
  let (lower, upper) = equalRange(arr, target)
  upper - lower

proc findFirst*[T](arr: openArray[T], target: T): int =
  ## Find first occurrence of target
  ## Returns -1 if not found
  let lower = lowerBound(arr, target)
  if lower < arr.len and arr[lower] == target:
    return lower
  -1

proc findLast*[T](arr: openArray[T], target: T): int =
  ## Find last occurrence of target
  ## Returns -1 if not found
  let upper = upperBound(arr, target)
  if upper > 0 and arr[upper - 1] == target:
    return upper - 1
  -1

when isMainModule:
  let arr = @[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  assert binarySearch(arr, 5) == 4
  assert binarySearch(arr, 1) == 0
  assert binarySearch(arr, 10) == 9
  assert binarySearch(arr, 11) == -1

  let arr2 = @[1, 2, 2, 2, 3, 4, 5]
  assert lowerBound(arr2, 2) == 1
  assert upperBound(arr2, 2) == 4
  assert countEqual(arr2, 2) == 3
  assert findFirst(arr2, 2) == 1
  assert findLast(arr2, 2) == 3
  echo "BinarySearch tests passed!"
