package algorithms

import scala.reflect.ClassTag

/**
 * Merge sort - stable, divide-and-conquer sorting algorithm.
 * Time: O(n log n)
 * Space: O(n)
 */
object MergeSort:
  def sort[T: ClassTag](arr: Array[T])(using ord: Ordering[T]): Array[T] =
    if arr.length <= 1 then arr
    else
      mergeSort(arr, 0, arr.length - 1)
      arr

  private def mergeSort[T: ClassTag](arr: Array[T], left: Int, right: Int)(using ord: Ordering[T]): Unit =
    if left < right then
      val mid = left + (right - left) / 2
      mergeSort(arr, left, mid)
      mergeSort(arr, mid + 1, right)
      merge(arr, left, mid, right)

  private def merge[T: ClassTag](arr: Array[T], left: Int, mid: Int, right: Int)(using ord: Ordering[T]): Unit =
    val leftArr = arr.slice(left, mid + 1)
    val rightArr = arr.slice(mid + 1, right + 1)

    var i = 0
    var j = 0
    var k = left

    while i < leftArr.length && j < rightArr.length do
      if ord.lteq(leftArr(i), rightArr(j)) then
        arr(k) = leftArr(i)
        i += 1
      else
        arr(k) = rightArr(j)
        j += 1
      k += 1

    while i < leftArr.length do
      arr(k) = leftArr(i)
      i += 1
      k += 1

    while j < rightArr.length do
      arr(k) = rightArr(j)
      j += 1
      k += 1

  def sorted[T: ClassTag](arr: Array[T])(using ord: Ordering[T]): Array[T] =
    val copy = arr.clone()
    sort(copy)
