## Doubly Linked List - Bidirectional linked list
##
## Time Complexity:
## - prepend: O(1)
## - append: O(1)
## - removeFirst: O(1)
## - removeLast: O(1)
## - get: O(n)
## - removeAt: O(n)

type
  DNode[T] = ref object
    data: T
    prev: DNode[T]
    next: DNode[T]

  DoublyLinkedList*[T] = object
    head: DNode[T]
    tail: DNode[T]
    count: int

proc newDoublyLinkedList*[T](): DoublyLinkedList[T] =
  ## Create a new empty doubly linked list
  DoublyLinkedList[T](head: nil, tail: nil, count: 0)

proc prepend*[T](list: var DoublyLinkedList[T], item: T) =
  ## Add an item to the front
  let node = DNode[T](data: item, prev: nil, next: list.head)
  if list.head != nil:
    list.head.prev = node
  list.head = node
  if list.tail == nil:
    list.tail = node
  inc list.count

proc append*[T](list: var DoublyLinkedList[T], item: T) =
  ## Add an item to the back
  let node = DNode[T](data: item, prev: list.tail, next: nil)
  if list.tail != nil:
    list.tail.next = node
  list.tail = node
  if list.head == nil:
    list.head = node
  inc list.count

proc get*[T](list: DoublyLinkedList[T], index: int): T =
  ## Get item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= list.count:
    raise newException(IndexDefect, "Index out of bounds")
  var current: DNode[T]
  if index < list.count div 2:
    current = list.head
    for _ in 0 ..< index:
      current = current.next
  else:
    current = list.tail
    for _ in 0 ..< list.count - 1 - index:
      current = current.prev
  current.data

proc `[]`*[T](list: DoublyLinkedList[T], index: int): T =
  ## Get item at index using [] operator
  list.get(index)

proc set*[T](list: var DoublyLinkedList[T], index: int, item: T) =
  ## Set item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= list.count:
    raise newException(IndexDefect, "Index out of bounds")
  var current: DNode[T]
  if index < list.count div 2:
    current = list.head
    for _ in 0 ..< index:
      current = current.next
  else:
    current = list.tail
    for _ in 0 ..< list.count - 1 - index:
      current = current.prev
  current.data = item

proc removeFirst*[T](list: var DoublyLinkedList[T]): T =
  ## Remove and return the first item
  ## Raises IndexDefect if list is empty
  if list.head == nil:
    raise newException(IndexDefect, "List is empty")
  result = list.head.data
  list.head = list.head.next
  if list.head != nil:
    list.head.prev = nil
  else:
    list.tail = nil
  dec list.count

proc removeLast*[T](list: var DoublyLinkedList[T]): T =
  ## Remove and return the last item
  ## Raises IndexDefect if list is empty
  if list.tail == nil:
    raise newException(IndexDefect, "List is empty")
  result = list.tail.data
  list.tail = list.tail.prev
  if list.tail != nil:
    list.tail.next = nil
  else:
    list.head = nil
  dec list.count

proc removeAt*[T](list: var DoublyLinkedList[T], index: int): T =
  ## Remove and return item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= list.count:
    raise newException(IndexDefect, "Index out of bounds")
  if index == 0:
    return list.removeFirst()
  if index == list.count - 1:
    return list.removeLast()
  var current: DNode[T]
  if index < list.count div 2:
    current = list.head
    for _ in 0 ..< index:
      current = current.next
  else:
    current = list.tail
    for _ in 0 ..< list.count - 1 - index:
      current = current.prev
  result = current.data
  current.prev.next = current.next
  current.next.prev = current.prev
  dec list.count

proc find*[T](list: DoublyLinkedList[T], item: T): int =
  ## Find index of item, returns -1 if not found
  var current = list.head
  var index = 0
  while current != nil:
    if current.data == item:
      return index
    current = current.next
    inc index
  -1

proc contains*[T](list: DoublyLinkedList[T], item: T): bool =
  ## Check if list contains item
  list.find(item) != -1

proc isEmpty*[T](list: DoublyLinkedList[T]): bool =
  ## Check if the list is empty
  list.count == 0

proc size*[T](list: DoublyLinkedList[T]): int =
  ## Return the number of items
  list.count

proc len*[T](list: DoublyLinkedList[T]): int =
  ## Return the number of items (alias for size)
  list.count

proc clear*[T](list: var DoublyLinkedList[T]) =
  ## Remove all items
  list.head = nil
  list.tail = nil
  list.count = 0

proc toSeq*[T](list: DoublyLinkedList[T]): seq[T] =
  ## Convert to sequence
  result = @[]
  var current = list.head
  while current != nil:
    result.add(current.data)
    current = current.next

proc toSeqReverse*[T](list: DoublyLinkedList[T]): seq[T] =
  ## Convert to sequence in reverse order
  result = @[]
  var current = list.tail
  while current != nil:
    result.add(current.data)
    current = current.prev

iterator items*[T](list: DoublyLinkedList[T]): T =
  ## Iterate over items forward
  var current = list.head
  while current != nil:
    yield current.data
    current = current.next

iterator itemsReverse*[T](list: DoublyLinkedList[T]): T =
  ## Iterate over items backward
  var current = list.tail
  while current != nil:
    yield current.data
    current = current.prev

when isMainModule:
  var list = newDoublyLinkedList[int]()
  list.append(1)
  list.append(2)
  list.prepend(0)
  assert list[0] == 0
  assert list[1] == 1
  assert list[2] == 2
  assert list.size() == 3
  assert list.removeFirst() == 0
  assert list.removeLast() == 2
  assert list.size() == 1
  echo "DoublyLinkedList tests passed!"
