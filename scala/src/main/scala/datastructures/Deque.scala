package datastructures

/**
 * Double-ended queue supporting operations at both ends.
 * Time: O(1) for all operations
 * Space: O(n)
 */
class Deque[T]:
  private case class Node(value: T, var prev: Node = null, var next: Node = null)

  private var head: Node = null
  private var tail: Node = null
  private var count = 0

  def pushFront(value: T): this.type =
    val node = Node(value, next = head)
    if head != null then head.prev = node
    else tail = node
    head = node
    count += 1
    this

  def pushBack(value: T): this.type =
    val node = Node(value, prev = tail)
    if tail != null then tail.next = node
    else head = node
    tail = node
    count += 1
    this

  def popFront(): Option[T] =
    if head == null then None
    else
      val value = head.value
      head = head.next
      if head != null then head.prev = null
      else tail = null
      count -= 1
      Some(value)

  def popBack(): Option[T] =
    if tail == null then None
    else
      val value = tail.value
      tail = tail.prev
      if tail != null then tail.next = null
      else head = null
      count -= 1
      Some(value)

  def peekFront: Option[T] = Option(head).map(_.value)

  def peekBack: Option[T] = Option(tail).map(_.value)

  def isEmpty: Boolean = count == 0

  def size: Int = count

  def clear(): this.type =
    head = null
    tail = null
    count = 0
    this
