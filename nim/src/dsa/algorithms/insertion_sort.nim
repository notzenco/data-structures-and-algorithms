## Insertion Sort - Simple stable sorting algorithm
##
## Time Complexity: O(n^2)
## Space Complexity: O(1)
## Stable: Yes

proc insertionSort*[T](arr: var openArray[T]) =
  ## Sort array in-place using insertion sort
  for i in 1 ..< arr.len:
    let key = arr[i]
    var j = i - 1
    while j >= 0 and arr[j] > key:
      arr[j + 1] = arr[j]
      dec j
    arr[j + 1] = key

proc insertionSortDesc*[T](arr: var openArray[T]) =
  ## Sort array in-place in descending order
  for i in 1 ..< arr.len:
    let key = arr[i]
    var j = i - 1
    while j >= 0 and arr[j] < key:
      arr[j + 1] = arr[j]
      dec j
    arr[j + 1] = key

proc insertionSortBy*[T, K](arr: var openArray[T], keyFunc: proc(x: T): K) =
  ## Sort array in-place by key function
  for i in 1 ..< arr.len:
    let key = arr[i]
    let keyVal = keyFunc(key)
    var j = i - 1
    while j >= 0 and keyFunc(arr[j]) > keyVal:
      arr[j + 1] = arr[j]
      dec j
    arr[j + 1] = key

proc insertionSortSlice*[T](arr: var openArray[T], start, stop: int) =
  ## Sort a slice of array in-place
  let actualStop = min(stop, arr.len)
  for i in (start + 1) ..< actualStop:
    let key = arr[i]
    var j = i - 1
    while j >= start and arr[j] > key:
      arr[j + 1] = arr[j]
      dec j
    arr[j + 1] = key

proc sorted*[T](arr: openArray[T]): seq[T] =
  ## Return a sorted copy of the array
  result = @arr
  insertionSort(result)

proc sortedDesc*[T](arr: openArray[T]): seq[T] =
  ## Return a sorted copy in descending order
  result = @arr
  insertionSortDesc(result)

proc isSorted*[T](arr: openArray[T]): bool =
  ## Check if array is sorted in ascending order
  for i in 1 ..< arr.len:
    if arr[i] < arr[i - 1]:
      return false
  true

proc isSortedDesc*[T](arr: openArray[T]): bool =
  ## Check if array is sorted in descending order
  for i in 1 ..< arr.len:
    if arr[i] > arr[i - 1]:
      return false
  true

when isMainModule:
  var arr = @[5, 2, 8, 1, 9, 3]
  insertionSort(arr)
  assert arr == @[1, 2, 3, 5, 8, 9]
  assert isSorted(arr)

  var arr2 = @[5, 2, 8, 1, 9, 3]
  insertionSortDesc(arr2)
  assert arr2 == @[9, 8, 5, 3, 2, 1]
  assert isSortedDesc(arr2)

  let arr3 = sorted(@[3, 1, 4, 1, 5, 9, 2, 6])
  assert arr3 == @[1, 1, 2, 3, 4, 5, 6, 9]
  echo "InsertionSort tests passed!"
