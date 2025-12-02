## Deque - Double-ended queue
##
## Time Complexity:
## - pushFront: O(1) amortized
## - pushBack: O(1) amortized
## - popFront: O(1) amortized
## - popBack: O(1)
## - peekFront: O(1)
## - peekBack: O(1)

import std/deques

type
  Deque*[T] = object
    items: std.deques.Deque[T]

proc newDeque*[T](): Deque[T] =
  ## Create a new empty deque
  Deque[T](items: initDeque[T]())

proc newDeque*[T](capacity: int): Deque[T] =
  ## Create a new deque with initial capacity
  Deque[T](items: initDeque[T](capacity))

proc pushFront*[T](d: var Deque[T], item: T) =
  ## Add an item to the front
  d.items.addFirst(item)

proc pushBack*[T](d: var Deque[T], item: T) =
  ## Add an item to the back
  d.items.addLast(item)

proc popFront*[T](d: var Deque[T]): T =
  ## Remove and return the front item
  ## Raises IndexDefect if deque is empty
  if d.items.len == 0:
    raise newException(IndexDefect, "Deque is empty")
  d.items.popFirst()

proc popBack*[T](d: var Deque[T]): T =
  ## Remove and return the back item
  ## Raises IndexDefect if deque is empty
  if d.items.len == 0:
    raise newException(IndexDefect, "Deque is empty")
  d.items.popLast()

proc peekFront*[T](d: Deque[T]): T =
  ## Return the front item without removing it
  ## Raises IndexDefect if deque is empty
  if d.items.len == 0:
    raise newException(IndexDefect, "Deque is empty")
  d.items.peekFirst()

proc peekBack*[T](d: Deque[T]): T =
  ## Return the back item without removing it
  ## Raises IndexDefect if deque is empty
  if d.items.len == 0:
    raise newException(IndexDefect, "Deque is empty")
  d.items.peekLast()

proc get*[T](d: Deque[T], index: int): T =
  ## Get item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= d.items.len:
    raise newException(IndexDefect, "Index out of bounds")
  d.items[index]

proc `[]`*[T](d: Deque[T], index: int): T =
  ## Get item at index using [] operator
  d.get(index)

proc isEmpty*[T](d: Deque[T]): bool =
  ## Check if the deque is empty
  d.items.len == 0

proc size*[T](d: Deque[T]): int =
  ## Return the number of items
  d.items.len

proc len*[T](d: Deque[T]): int =
  ## Return the number of items (alias for size)
  d.items.len

proc clear*[T](d: var Deque[T]) =
  ## Remove all items
  d.items.clear()

proc toSeq*[T](d: Deque[T]): seq[T] =
  ## Convert to sequence
  result = @[]
  for item in d.items:
    result.add(item)

iterator items*[T](d: Deque[T]): T =
  ## Iterate over items front to back
  for item in d.items:
    yield item

when isMainModule:
  var d = newDeque[int]()
  d.pushBack(1)
  d.pushBack(2)
  d.pushFront(0)
  assert d.peekFront() == 0
  assert d.peekBack() == 2
  assert d.popFront() == 0
  assert d.popBack() == 2
  assert d.size() == 1
  echo "Deque tests passed!"
