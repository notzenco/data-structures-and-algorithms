package datastructures

/**
 * Doubly linked list with bidirectional traversal.
 * Time: O(1) for prepend/append/removeFirst/removeLast, O(n) for arbitrary access
 * Space: O(n)
 */
class DoublyLinkedList[T]:
  private case class Node(var value: T, var prev: Node = null, var next: Node = null)

  private var head: Node = null
  private var tail: Node = null
  private var count = 0

  def prepend(value: T): this.type =
    val node = Node(value, next = head)
    if head != null then head.prev = node
    else tail = node
    head = node
    count += 1
    this

  def append(value: T): this.type =
    val node = Node(value, prev = tail)
    if tail != null then tail.next = node
    else head = node
    tail = node
    count += 1
    this

  def removeFirst(): Option[T] =
    if head == null then None
    else
      val value = head.value
      head = head.next
      if head != null then head.prev = null
      else tail = null
      count -= 1
      Some(value)

  def removeLast(): Option[T] =
    if tail == null then None
    else
      val value = tail.value
      tail = tail.prev
      if tail != null then tail.next = null
      else head = null
      count -= 1
      Some(value)

  def first: Option[T] = Option(head).map(_.value)

  def last: Option[T] = Option(tail).map(_.value)

  def get(index: Int): Option[T] =
    if index < 0 || index >= count then None
    else
      val node = if index < count / 2 then
        var current = head
        for _ <- 0 until index do current = current.next
        current
      else
        var current = tail
        for _ <- count - 1 until index by -1 do current = current.prev
        current
      Some(node.value)

  def indexOf(value: T): Int =
    var current = head
    var index = 0
    while current != null do
      if current.value == value then return index
      current = current.next
      index += 1
    -1

  def contains(value: T): Boolean = indexOf(value) != -1

  def isEmpty: Boolean = count == 0

  def size: Int = count

  def clear(): this.type =
    head = null
    tail = null
    count = 0
    this

  def toList: List[T] =
    val builder = List.newBuilder[T]
    var current = head
    while current != null do
      builder += current.value
      current = current.next
    builder.result()
