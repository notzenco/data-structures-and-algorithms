package datastructures

/**
 * Singly linked list with forward traversal.
 * Time: O(1) for prepend, O(n) for other operations
 * Space: O(n)
 */
class SinglyLinkedList[T]:
  private case class Node(var value: T, var next: Node = null)

  private var head: Node = null
  private var count = 0

  def prepend(value: T): this.type =
    head = Node(value, head)
    count += 1
    this

  def append(value: T): this.type =
    val node = Node(value)
    if head == null then head = node
    else
      var current = head
      while current.next != null do current = current.next
      current.next = node
    count += 1
    this

  def insert(index: Int, value: T): Boolean =
    if index < 0 || index > count then false
    else if index == 0 then
      prepend(value)
      true
    else
      var current = head
      for _ <- 0 until index - 1 do current = current.next
      current.next = Node(value, current.next)
      count += 1
      true

  def removeAt(index: Int): Option[T] =
    if index < 0 || index >= count then None
    else if index == 0 then
      val value = head.value
      head = head.next
      count -= 1
      Some(value)
    else
      var current = head
      for _ <- 0 until index - 1 do current = current.next
      val value = current.next.value
      current.next = current.next.next
      count -= 1
      Some(value)

  def get(index: Int): Option[T] =
    if index < 0 || index >= count then None
    else
      var current = head
      for _ <- 0 until index do current = current.next
      Some(current.value)

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
    count = 0
    this

  def toList: List[T] =
    val builder = List.newBuilder[T]
    var current = head
    while current != null do
      builder += current.value
      current = current.next
    builder.result()
