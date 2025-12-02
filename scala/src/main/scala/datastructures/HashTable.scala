package datastructures

/**
 * Hash table implementation with open addressing and linear probing.
 * Time: O(1) average for all operations
 * Space: O(n)
 */
class HashTable[K, V](initialCapacity: Int = 16):
  private case class Entry(key: K, var value: V, var deleted: Boolean = false)

  private val LoadFactor = 0.7
  private var capacity = math.max(1, initialCapacity)
  private var buckets: Array[Entry] = new Array[Entry](capacity)
  private var count = 0

  def put(key: K, value: V): this.type =
    if count >= capacity * LoadFactor then resize(capacity * 2)

    var index = hash(key)
    var firstDeleted = -1

    for _ <- 0 until capacity do
      val entry = buckets(index)

      if entry == null then
        val insertIndex = if firstDeleted != -1 then firstDeleted else index
        buckets(insertIndex) = Entry(key, value)
        count += 1
        return this
      else if entry.deleted && firstDeleted == -1 then
        firstDeleted = index
      else if !entry.deleted && entry.key == key then
        entry.value = value
        return this

      index = (index + 1) % capacity

    if firstDeleted != -1 then
      buckets(firstDeleted) = Entry(key, value)
      count += 1

    this

  def get(key: K): Option[V] =
    findIndex(key) match
      case -1 => None
      case idx => Some(buckets(idx).value)

  def remove(key: K): Option[V] =
    findIndex(key) match
      case -1 => None
      case idx =>
        val entry = buckets(idx)
        val value = entry.value
        entry.deleted = true
        count -= 1
        Some(value)

  def contains(key: K): Boolean = findIndex(key) != -1

  def isEmpty: Boolean = count == 0

  def size: Int = count

  def keys: List[K] =
    buckets.filter(e => e != null && !e.deleted).map(_.key).toList

  def values: List[V] =
    buckets.filter(e => e != null && !e.deleted).map(_.value).toList

  def clear(): this.type =
    buckets = new Array[Entry](capacity)
    count = 0
    this

  private def hash(key: K): Int = (key.hashCode().abs) % capacity

  private def findIndex(key: K): Int =
    var index = hash(key)
    for _ <- 0 until capacity do
      val entry = buckets(index)
      if entry == null then return -1
      if !entry.deleted && entry.key == key then return index
      index = (index + 1) % capacity
    -1

  private def resize(newCapacity: Int): Unit =
    val oldBuckets = buckets
    capacity = newCapacity
    buckets = new Array[Entry](capacity)
    count = 0
    for entry <- oldBuckets if entry != null && !entry.deleted do
      put(entry.key, entry.value)
