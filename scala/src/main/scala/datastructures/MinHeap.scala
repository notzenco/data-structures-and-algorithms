package datastructures

import scala.collection.mutable.ArrayBuffer

/**
 * Min-heap implementation with custom comparator support.
 * Time: O(log n) for insert/extract, O(1) for peek
 * Space: O(n)
 */
class MinHeap[T](using ord: Ordering[T]):
  private val items = ArrayBuffer.empty[T]

  def insert(value: T): this.type =
    items += value
    siftUp(items.length - 1)
    this

  def extractMin(): Option[T] =
    if items.isEmpty then None
    else
      val min = items(0)
      val last = items.remove(items.length - 1)
      if items.nonEmpty then
        items(0) = last
        siftDown(0)
      Some(min)

  def peek: Option[T] =
    if items.isEmpty then None
    else Some(items(0))

  def isEmpty: Boolean = items.isEmpty

  def size: Int = items.length

  def clear(): this.type =
    items.clear()
    this

  def toList: List[T] = items.toList

  private def siftUp(index: Int): Unit =
    var i = index
    while i > 0 do
      val parent = (i - 1) / 2
      if ord.lt(items(i), items(parent)) then
        swap(i, parent)
        i = parent
      else
        return

  private def siftDown(index: Int): Unit =
    var i = index
    while true do
      val left = 2 * i + 1
      val right = 2 * i + 2
      var smallest = i

      if left < items.length && ord.lt(items(left), items(smallest)) then
        smallest = left
      if right < items.length && ord.lt(items(right), items(smallest)) then
        smallest = right

      if smallest != i then
        swap(i, smallest)
        i = smallest
      else
        return

  private def swap(i: Int, j: Int): Unit =
    val temp = items(i)
    items(i) = items(j)
    items(j) = temp

object MinHeap:
  def from[T: Ordering](elements: Iterable[T]): MinHeap[T] =
    val heap = new MinHeap[T]
    for elem <- elements do heap.insert(elem)
    heap
