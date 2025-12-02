## Hash Table - Open addressing with linear probing
##
## Time Complexity (average):
## - put: O(1)
## - get: O(1)
## - remove: O(1)
## - contains: O(1)

import std/hashes

type
  EntryState = enum
    Empty, Occupied, Deleted

  Entry[K, V] = object
    key: K
    value: V
    state: EntryState

  HashTable*[K, V] = object
    buckets: seq[Entry[K, V]]
    count: int
    capacity: int

const
  InitialCapacity = 16
  LoadFactorThreshold = 0.75

proc newHashTable*[K, V](): HashTable[K, V] =
  ## Create a new empty hash table
  result.capacity = InitialCapacity
  result.buckets = newSeq[Entry[K, V]](InitialCapacity)
  result.count = 0

proc findSlot[K, V](ht: HashTable[K, V], key: K): int =
  ## Find the slot for a key
  let h = hash(key) and (ht.capacity - 1)
  var index = h
  var firstDeleted = -1
  while true:
    case ht.buckets[index].state
    of Empty:
      if firstDeleted != -1:
        return firstDeleted
      return index
    of Deleted:
      if firstDeleted == -1:
        firstDeleted = index
    of Occupied:
      if ht.buckets[index].key == key:
        return index
    index = (index + 1) and (ht.capacity - 1)
    if index == h:
      break
  if firstDeleted != -1:
    return firstDeleted
  return -1

proc resize[K, V](ht: var HashTable[K, V]) =
  ## Resize the hash table
  let oldBuckets = ht.buckets
  ht.capacity = ht.capacity * 2
  ht.buckets = newSeq[Entry[K, V]](ht.capacity)
  ht.count = 0
  for entry in oldBuckets:
    if entry.state == Occupied:
      let index = ht.findSlot(entry.key)
      ht.buckets[index] = Entry[K, V](key: entry.key, value: entry.value, state: Occupied)
      inc ht.count

proc put*[K, V](ht: var HashTable[K, V], key: K, value: V) =
  ## Insert or update a key-value pair
  if float(ht.count + 1) / float(ht.capacity) > LoadFactorThreshold:
    ht.resize()
  let index = ht.findSlot(key)
  if ht.buckets[index].state != Occupied:
    inc ht.count
  ht.buckets[index] = Entry[K, V](key: key, value: value, state: Occupied)

proc `[]=`*[K, V](ht: var HashTable[K, V], key: K, value: V) =
  ## Insert or update using []= operator
  ht.put(key, value)

proc get*[K, V](ht: HashTable[K, V], key: K): V =
  ## Get value by key
  ## Raises KeyError if key not found
  let index = ht.findSlot(key)
  if index == -1 or ht.buckets[index].state != Occupied or ht.buckets[index].key != key:
    raise newException(KeyError, "Key not found")
  ht.buckets[index].value

proc getOrDefault*[K, V](ht: HashTable[K, V], key: K, default: V): V =
  ## Get value by key, or return default if not found
  let index = ht.findSlot(key)
  if index == -1 or ht.buckets[index].state != Occupied or ht.buckets[index].key != key:
    return default
  ht.buckets[index].value

proc `[]`*[K, V](ht: HashTable[K, V], key: K): V =
  ## Get value using [] operator
  ht.get(key)

proc contains*[K, V](ht: HashTable[K, V], key: K): bool =
  ## Check if key exists
  let index = ht.findSlot(key)
  index != -1 and ht.buckets[index].state == Occupied and ht.buckets[index].key == key

proc hasKey*[K, V](ht: HashTable[K, V], key: K): bool =
  ## Check if key exists (alias for contains)
  ht.contains(key)

proc remove*[K, V](ht: var HashTable[K, V], key: K): bool =
  ## Remove a key-value pair, returns true if found
  let index = ht.findSlot(key)
  if index == -1 or ht.buckets[index].state != Occupied or ht.buckets[index].key != key:
    return false
  ht.buckets[index].state = Deleted
  dec ht.count
  true

proc delete*[K, V](ht: var HashTable[K, V], key: K) =
  ## Remove a key-value pair
  ## Raises KeyError if key not found
  if not ht.remove(key):
    raise newException(KeyError, "Key not found")

proc isEmpty*[K, V](ht: HashTable[K, V]): bool =
  ## Check if the hash table is empty
  ht.count == 0

proc size*[K, V](ht: HashTable[K, V]): int =
  ## Return the number of entries
  ht.count

proc len*[K, V](ht: HashTable[K, V]): int =
  ## Return the number of entries (alias for size)
  ht.count

proc clear*[K, V](ht: var HashTable[K, V]) =
  ## Remove all entries
  ht.buckets = newSeq[Entry[K, V]](ht.capacity)
  ht.count = 0

proc keys*[K, V](ht: HashTable[K, V]): seq[K] =
  ## Get all keys
  result = @[]
  for entry in ht.buckets:
    if entry.state == Occupied:
      result.add(entry.key)

proc values*[K, V](ht: HashTable[K, V]): seq[V] =
  ## Get all values
  result = @[]
  for entry in ht.buckets:
    if entry.state == Occupied:
      result.add(entry.value)

iterator pairs*[K, V](ht: HashTable[K, V]): (K, V) =
  ## Iterate over key-value pairs
  for entry in ht.buckets:
    if entry.state == Occupied:
      yield (entry.key, entry.value)

when isMainModule:
  var ht = newHashTable[string, int]()
  ht["one"] = 1
  ht["two"] = 2
  ht["three"] = 3
  assert ht["one"] == 1
  assert ht["two"] == 2
  assert ht.contains("three")
  assert not ht.contains("four")
  assert ht.size() == 3
  assert ht.remove("two")
  assert not ht.contains("two")
  assert ht.size() == 2
  echo "HashTable tests passed!"
