package datastructures

/**
 * FIFO (First In, First Out) data structure using a linked list.
 * Time: O(1) for all operations
 * Space: O(n)
 */
class Queue[T]:
  private case class Node(value: T, var next: Node = null)

  private var head: Node = null
  private var tail: Node = null
  private var count = 0

  def enqueue(value: T): this.type =
    val node = Node(value)
    if tail != null then tail.next = node
    else head = node
    tail = node
    count += 1
    this

  def dequeue(): Option[T] =
    if head == null then None
    else
      val value = head.value
      head = head.next
      if head == null then tail = null
      count -= 1
      Some(value)

  def peek: Option[T] = Option(head).map(_.value)

  def isEmpty: Boolean = count == 0

  def size: Int = count

  def clear(): this.type =
    head = null
    tail = null
    count = 0
    this
