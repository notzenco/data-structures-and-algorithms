package datastructures

import scala.collection.mutable.ArrayBuffer

/**
 * LIFO (Last In, First Out) data structure.
 * Time: O(1) for all operations
 * Space: O(n)
 */
class Stack[T]:
  private val items = ArrayBuffer.empty[T]

  def push(value: T): this.type =
    items += value
    this

  def pop(): Option[T] =
    if items.isEmpty then None
    else Some(items.remove(items.length - 1))

  def peek: Option[T] = items.lastOption

  def isEmpty: Boolean = items.isEmpty

  def size: Int = items.length

  def clear(): this.type =
    items.clear()
    this

  def toList: List[T] = items.toList
