package datastructures

import scala.collection.mutable.ArrayBuffer

/**
 * Resizable array with automatic capacity management.
 * Time: O(1) amortized for push, O(n) for insert/remove
 * Space: O(n)
 */
class DynamicArray[T]:
  private val items = ArrayBuffer.empty[T]

  def push(value: T): this.type =
    items += value
    this

  def pop(): Option[T] =
    if items.isEmpty then None
    else Some(items.remove(items.length - 1))

  def get(index: Int): Option[T] =
    if index < 0 || index >= items.length then None
    else Some(items(index))

  def set(index: Int, value: T): Boolean =
    if index < 0 || index >= items.length then false
    else
      items(index) = value
      true

  def insert(index: Int, value: T): Boolean =
    if index < 0 || index > items.length then false
    else
      items.insert(index, value)
      true

  def removeAt(index: Int): Option[T] =
    if index < 0 || index >= items.length then None
    else Some(items.remove(index))

  def indexOf(value: T): Int = items.indexOf(value)

  def contains(value: T): Boolean = items.contains(value)

  def isEmpty: Boolean = items.isEmpty

  def size: Int = items.length

  def clear(): this.type =
    items.clear()
    this

  def toList: List[T] = items.toList
