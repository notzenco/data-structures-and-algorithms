package algorithms

import scala.reflect.ClassTag

/**
 * Quick sort with median-of-three pivot selection.
 * Time: O(n log n) average, O(n²) worst case
 * Space: O(log n) stack space
 */
object QuickSort:
  def sort[T: ClassTag](arr: Array[T])(using ord: Ordering[T]): Array[T] =
    if arr.length <= 1 then arr
    else
      quickSort(arr, 0, arr.length - 1)
      arr

  private def quickSort[T](arr: Array[T], low: Int, high: Int)(using ord: Ordering[T]): Unit =
    if low < high then
      val pivotIndex = partition(arr, low, high)
      quickSort(arr, low, pivotIndex - 1)
      quickSort(arr, pivotIndex + 1, high)

  private def partition[T](arr: Array[T], low: Int, high: Int)(using ord: Ordering[T]): Int =
    val pivotIndex = medianOfThree(arr, low, high)
    swap(arr, pivotIndex, high)
    val pivot = arr(high)

    var i = low - 1
    for j <- low until high do
      if ord.lteq(arr(j), pivot) then
        i += 1
        swap(arr, i, j)

    swap(arr, i + 1, high)
    i + 1

  private def medianOfThree[T](arr: Array[T], low: Int, high: Int)(using ord: Ordering[T]): Int =
    val mid = low + (high - low) / 2

    if ord.gt(arr(low), arr(mid)) then swap(arr, low, mid)
    if ord.gt(arr(low), arr(high)) then swap(arr, low, high)
    if ord.gt(arr(mid), arr(high)) then swap(arr, mid, high)

    mid

  private def swap[T](arr: Array[T], i: Int, j: Int): Unit =
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp

  def sorted[T: ClassTag](arr: Array[T])(using ord: Ordering[T]): Array[T] =
    val copy = arr.clone()
    sort(copy)
