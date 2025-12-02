package algorithms

import scala.reflect.ClassTag

/**
 * Insertion sort - stable, in-place sorting algorithm.
 * Time: O(n²) average/worst, O(n) best
 * Space: O(1)
 */
object InsertionSort:
  def sort[T: ClassTag](arr: Array[T])(using ord: Ordering[T]): Array[T] =
    for i <- 1 until arr.length do
      val key = arr(i)
      var j = i - 1

      while j >= 0 && ord.gt(arr(j), key) do
        arr(j + 1) = arr(j)
        j -= 1

      arr(j + 1) = key

    arr

  def sorted[T: ClassTag](arr: Array[T])(using ord: Ordering[T]): Array[T] =
    val copy = arr.clone()
    sort(copy)
