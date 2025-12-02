## Merge Sort - Divide and conquer stable sorting
##
## Time Complexity: O(n log n)
## Space Complexity: O(n)
## Stable: Yes

proc merge[T](arr: var openArray[T], left, mid, right: int) =
  let leftSize = mid - left + 1
  let rightSize = right - mid

  var leftArr = newSeq[T](leftSize)
  var rightArr = newSeq[T](rightSize)

  for i in 0 ..< leftSize:
    leftArr[i] = arr[left + i]
  for i in 0 ..< rightSize:
    rightArr[i] = arr[mid + 1 + i]

  var i = 0
  var j = 0
  var k = left

  while i < leftSize and j < rightSize:
    if leftArr[i] <= rightArr[j]:
      arr[k] = leftArr[i]
      inc i
    else:
      arr[k] = rightArr[j]
      inc j
    inc k

  while i < leftSize:
    arr[k] = leftArr[i]
    inc i
    inc k

  while j < rightSize:
    arr[k] = rightArr[j]
    inc j
    inc k

proc mergeSortRange[T](arr: var openArray[T], left, right: int) =
  if left < right:
    let mid = left + (right - left) div 2
    mergeSortRange(arr, left, mid)
    mergeSortRange(arr, mid + 1, right)
    merge(arr, left, mid, right)

proc mergeSort*[T](arr: var openArray[T]) =
  ## Sort array in-place using merge sort
  if arr.len > 1:
    mergeSortRange(arr, 0, arr.len - 1)

proc mergeSortIterative*[T](arr: var openArray[T]) =
  ## Sort array using iterative (bottom-up) merge sort
  let n = arr.len
  var size = 1
  while size < n:
    var left = 0
    while left < n - size:
      let mid = left + size - 1
      let right = min(left + 2 * size - 1, n - 1)
      merge(arr, left, mid, right)
      left += 2 * size
    size *= 2

proc sorted*[T](arr: openArray[T]): seq[T] =
  ## Return a sorted copy using merge sort
  result = @arr
  mergeSort(result)

proc mergeSortBy*[T, K](arr: var openArray[T], keyFunc: proc(x: T): K) =
  ## Sort array by key function using merge sort
  proc mergeBy(left, mid, right: int) =
    let leftSize = mid - left + 1
    let rightSize = right - mid

    var leftArr = newSeq[T](leftSize)
    var rightArr = newSeq[T](rightSize)

    for i in 0 ..< leftSize:
      leftArr[i] = arr[left + i]
    for i in 0 ..< rightSize:
      rightArr[i] = arr[mid + 1 + i]

    var i = 0
    var j = 0
    var k = left

    while i < leftSize and j < rightSize:
      if keyFunc(leftArr[i]) <= keyFunc(rightArr[j]):
        arr[k] = leftArr[i]
        inc i
      else:
        arr[k] = rightArr[j]
        inc j
      inc k

    while i < leftSize:
      arr[k] = leftArr[i]
      inc i
      inc k

    while j < rightSize:
      arr[k] = rightArr[j]
      inc j
      inc k

  proc sortBy(left, right: int) =
    if left < right:
      let mid = left + (right - left) div 2
      sortBy(left, mid)
      sortBy(mid + 1, right)
      mergeBy(left, mid, right)

  if arr.len > 1:
    sortBy(0, arr.len - 1)

when isMainModule:
  var arr = @[5, 2, 8, 1, 9, 3, 7, 4, 6]
  mergeSort(arr)
  assert arr == @[1, 2, 3, 4, 5, 6, 7, 8, 9]

  var arr2 = @[5, 2, 8, 1, 9, 3, 7, 4, 6]
  mergeSortIterative(arr2)
  assert arr2 == @[1, 2, 3, 4, 5, 6, 7, 8, 9]

  let arr3 = sorted(@[3, 1, 4, 1, 5, 9, 2, 6])
  assert arr3 == @[1, 1, 2, 3, 4, 5, 6, 9]
  echo "MergeSort tests passed!"
