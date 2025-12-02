package algorithms

/**
 * Binary search algorithms for sorted sequences.
 * Time: O(log n)
 * Space: O(1)
 */
object BinarySearch:
  def search[T](arr: IndexedSeq[T], target: T)(using ord: Ordering[T]): Int =
    var left = 0
    var right = arr.length - 1

    while left <= right do
      val mid = left + (right - left) / 2
      val cmp = ord.compare(arr(mid), target)

      if cmp == 0 then return mid
      else if cmp < 0 then left = mid + 1
      else right = mid - 1

    -1

  def lowerBound[T](arr: IndexedSeq[T], target: T)(using ord: Ordering[T]): Int =
    var left = 0
    var right = arr.length

    while left < right do
      val mid = left + (right - left) / 2
      if ord.lt(arr(mid), target) then left = mid + 1
      else right = mid

    left

  def upperBound[T](arr: IndexedSeq[T], target: T)(using ord: Ordering[T]): Int =
    var left = 0
    var right = arr.length

    while left < right do
      val mid = left + (right - left) / 2
      if ord.lteq(arr(mid), target) then left = mid + 1
      else right = mid

    left

  def contains[T](arr: IndexedSeq[T], target: T)(using Ordering[T]): Boolean =
    search(arr, target) != -1
