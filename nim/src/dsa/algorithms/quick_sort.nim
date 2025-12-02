## Quick Sort - Divide and conquer sorting with median-of-three pivot
##
## Time Complexity: O(n log n) average, O(n^2) worst case
## Space Complexity: O(log n)
## Stable: No

proc swap[T](arr: var openArray[T], i, j: int) =
  let temp = arr[i]
  arr[i] = arr[j]
  arr[j] = temp

proc medianOfThree[T](arr: var openArray[T], left, right: int): int =
  ## Select median of three as pivot and move to right-1
  let mid = left + (right - left) div 2

  # Sort left, mid, right
  if arr[left] > arr[mid]:
    swap(arr, left, mid)
  if arr[left] > arr[right]:
    swap(arr, left, right)
  if arr[mid] > arr[right]:
    swap(arr, mid, right)

  # Move median to right-1 (just before right)
  swap(arr, mid, right - 1)
  right - 1

proc partition[T](arr: var openArray[T], left, right: int): int =
  let pivotIndex = medianOfThree(arr, left, right)
  let pivot = arr[pivotIndex]

  var i = left
  var j = pivotIndex - 1

  while true:
    while arr[i] < pivot:
      inc i
    while j > left and arr[j] > pivot:
      dec j

    if i >= j:
      break

    swap(arr, i, j)
    inc i
    dec j

  swap(arr, i, pivotIndex)
  i

proc partitionSimple[T](arr: var openArray[T], left, right: int): int =
  ## Simple partition with rightmost element as pivot
  let pivot = arr[right]
  var i = left - 1

  for j in left ..< right:
    if arr[j] <= pivot:
      inc i
      swap(arr, i, j)

  swap(arr, i + 1, right)
  i + 1

proc quickSortRange[T](arr: var openArray[T], left, right: int) =
  if right - left < 10:
    # Use insertion sort for small arrays
    for i in (left + 1) .. right:
      let key = arr[i]
      var j = i - 1
      while j >= left and arr[j] > key:
        arr[j + 1] = arr[j]
        dec j
      arr[j + 1] = key
  elif left < right:
    let pivotIndex = partition(arr, left, right)
    quickSortRange(arr, left, pivotIndex - 1)
    quickSortRange(arr, pivotIndex + 1, right)

proc quickSort*[T](arr: var openArray[T]) =
  ## Sort array in-place using quick sort with median-of-three pivot
  if arr.len > 1:
    quickSortRange(arr, 0, arr.len - 1)

proc quickSortSimple*[T](arr: var openArray[T]) =
  ## Sort array using simple quick sort (rightmost pivot)
  proc sortRange(left, right: int) =
    if left < right:
      let pivotIndex = partitionSimple(arr, left, right)
      sortRange(left, pivotIndex - 1)
      sortRange(pivotIndex + 1, right)

  if arr.len > 1:
    sortRange(0, arr.len - 1)

proc sorted*[T](arr: openArray[T]): seq[T] =
  ## Return a sorted copy using quick sort
  result = @arr
  quickSort(result)

proc quickSelect*[T](arr: var openArray[T], k: int): T =
  ## Find the k-th smallest element (0-indexed)
  ## Modifies the array
  proc select(left, right, k: int): T =
    if left == right:
      return arr[left]

    let pivotIndex = partitionSimple(arr, left, right)

    if k == pivotIndex:
      return arr[k]
    elif k < pivotIndex:
      return select(left, pivotIndex - 1, k)
    else:
      return select(pivotIndex + 1, right, k)

  if k < 0 or k >= arr.len:
    raise newException(IndexDefect, "k out of bounds")
  select(0, arr.len - 1, k)

proc findMedian*[T](arr: var openArray[T]): T =
  ## Find median of array (modifies array)
  quickSelect(arr, arr.len div 2)

when isMainModule:
  var arr = @[5, 2, 8, 1, 9, 3, 7, 4, 6]
  quickSort(arr)
  assert arr == @[1, 2, 3, 4, 5, 6, 7, 8, 9]

  var arr2 = @[5, 2, 8, 1, 9, 3, 7, 4, 6]
  quickSortSimple(arr2)
  assert arr2 == @[1, 2, 3, 4, 5, 6, 7, 8, 9]

  var arr3 = @[5, 2, 8, 1, 9]
  assert quickSelect(arr3, 2) == 5  # 3rd smallest

  let arr4 = sorted(@[3, 1, 4, 1, 5, 9, 2, 6])
  assert arr4 == @[1, 1, 2, 3, 4, 5, 6, 9]
  echo "QuickSort tests passed!"
