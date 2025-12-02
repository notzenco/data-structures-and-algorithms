## Singly Linked List - Forward-only linked list
##
## Time Complexity:
## - prepend: O(1)
## - append: O(n) or O(1) with tail pointer
## - get: O(n)
## - removeAt: O(n)
## - find: O(n)

type
  Node[T] = ref object
    data: T
    next: Node[T]

  SinglyLinkedList*[T] = object
    head: Node[T]
    tail: Node[T]
    count: int

proc newSinglyLinkedList*[T](): SinglyLinkedList[T] =
  ## Create a new empty singly linked list
  SinglyLinkedList[T](head: nil, tail: nil, count: 0)

proc prepend*[T](list: var SinglyLinkedList[T], item: T) =
  ## Add an item to the front
  let node = Node[T](data: item, next: list.head)
  list.head = node
  if list.tail == nil:
    list.tail = node
  inc list.count

proc append*[T](list: var SinglyLinkedList[T], item: T) =
  ## Add an item to the back
  let node = Node[T](data: item, next: nil)
  if list.tail == nil:
    list.head = node
    list.tail = node
  else:
    list.tail.next = node
    list.tail = node
  inc list.count

proc get*[T](list: SinglyLinkedList[T], index: int): T =
  ## Get item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= list.count:
    raise newException(IndexDefect, "Index out of bounds")
  var current = list.head
  for _ in 0 ..< index:
    current = current.next
  current.data

proc `[]`*[T](list: SinglyLinkedList[T], index: int): T =
  ## Get item at index using [] operator
  list.get(index)

proc set*[T](list: var SinglyLinkedList[T], index: int, item: T) =
  ## Set item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= list.count:
    raise newException(IndexDefect, "Index out of bounds")
  var current = list.head
  for _ in 0 ..< index:
    current = current.next
  current.data = item

proc removeFirst*[T](list: var SinglyLinkedList[T]): T =
  ## Remove and return the first item
  ## Raises IndexDefect if list is empty
  if list.head == nil:
    raise newException(IndexDefect, "List is empty")
  result = list.head.data
  list.head = list.head.next
  if list.head == nil:
    list.tail = nil
  dec list.count

proc removeAt*[T](list: var SinglyLinkedList[T], index: int): T =
  ## Remove and return item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= list.count:
    raise newException(IndexDefect, "Index out of bounds")
  if index == 0:
    return list.removeFirst()
  var current = list.head
  for _ in 0 ..< index - 1:
    current = current.next
  result = current.next.data
  current.next = current.next.next
  if current.next == nil:
    list.tail = current
  dec list.count

proc find*[T](list: SinglyLinkedList[T], item: T): int =
  ## Find index of item, returns -1 if not found
  var current = list.head
  var index = 0
  while current != nil:
    if current.data == item:
      return index
    current = current.next
    inc index
  -1

proc contains*[T](list: SinglyLinkedList[T], item: T): bool =
  ## Check if list contains item
  list.find(item) != -1

proc isEmpty*[T](list: SinglyLinkedList[T]): bool =
  ## Check if the list is empty
  list.count == 0

proc size*[T](list: SinglyLinkedList[T]): int =
  ## Return the number of items
  list.count

proc len*[T](list: SinglyLinkedList[T]): int =
  ## Return the number of items (alias for size)
  list.count

proc clear*[T](list: var SinglyLinkedList[T]) =
  ## Remove all items
  list.head = nil
  list.tail = nil
  list.count = 0

proc toSeq*[T](list: SinglyLinkedList[T]): seq[T] =
  ## Convert to sequence
  result = @[]
  var current = list.head
  while current != nil:
    result.add(current.data)
    current = current.next

iterator items*[T](list: SinglyLinkedList[T]): T =
  ## Iterate over items
  var current = list.head
  while current != nil:
    yield current.data
    current = current.next

when isMainModule:
  var list = newSinglyLinkedList[int]()
  list.append(1)
  list.append(2)
  list.prepend(0)
  assert list[0] == 0
  assert list[1] == 1
  assert list[2] == 2
  assert list.size() == 3
  assert list.removeFirst() == 0
  assert list.size() == 2
  assert list.find(2) == 1
  assert list.contains(1)
  echo "SinglyLinkedList tests passed!"
