## Min Heap - Binary min heap / priority queue
##
## Time Complexity:
## - insert: O(log n)
## - extractMin: O(log n)
## - peek: O(1)
## - size: O(1)

type
  MinHeap*[T] = object
    items: seq[T]

proc newMinHeap*[T](): MinHeap[T] =
  ## Create a new empty min heap
  MinHeap[T](items: @[])

proc parent(i: int): int {.inline.} = (i - 1) div 2
proc leftChild(i: int): int {.inline.} = 2 * i + 1
proc rightChild(i: int): int {.inline.} = 2 * i + 2

proc swap[T](heap: var MinHeap[T], i, j: int) =
  let temp = heap.items[i]
  heap.items[i] = heap.items[j]
  heap.items[j] = temp

proc siftUp[T](heap: var MinHeap[T], index: int) =
  var i = index
  while i > 0 and heap.items[i] < heap.items[parent(i)]:
    heap.swap(i, parent(i))
    i = parent(i)

proc siftDown[T](heap: var MinHeap[T], index: int) =
  var smallest = index
  let left = leftChild(index)
  let right = rightChild(index)

  if left < heap.items.len and heap.items[left] < heap.items[smallest]:
    smallest = left
  if right < heap.items.len and heap.items[right] < heap.items[smallest]:
    smallest = right

  if smallest != index:
    heap.swap(index, smallest)
    heap.siftDown(smallest)

proc insert*[T](heap: var MinHeap[T], item: T) =
  ## Insert an item into the heap
  heap.items.add(item)
  heap.siftUp(heap.items.len - 1)

proc push*[T](heap: var MinHeap[T], item: T) =
  ## Insert an item into the heap (alias for insert)
  heap.insert(item)

proc extractMin*[T](heap: var MinHeap[T]): T =
  ## Remove and return the minimum item
  ## Raises IndexDefect if heap is empty
  if heap.items.len == 0:
    raise newException(IndexDefect, "Heap is empty")
  result = heap.items[0]
  heap.items[0] = heap.items[^1]
  heap.items.setLen(heap.items.len - 1)
  if heap.items.len > 0:
    heap.siftDown(0)

proc pop*[T](heap: var MinHeap[T]): T =
  ## Remove and return the minimum item (alias for extractMin)
  heap.extractMin()

proc peek*[T](heap: MinHeap[T]): T =
  ## Return the minimum item without removing it
  ## Raises IndexDefect if heap is empty
  if heap.items.len == 0:
    raise newException(IndexDefect, "Heap is empty")
  heap.items[0]

proc peekMin*[T](heap: MinHeap[T]): T =
  ## Return the minimum item without removing it (alias for peek)
  heap.peek()

proc isEmpty*[T](heap: MinHeap[T]): bool =
  ## Check if the heap is empty
  heap.items.len == 0

proc size*[T](heap: MinHeap[T]): int =
  ## Return the number of items
  heap.items.len

proc len*[T](heap: MinHeap[T]): int =
  ## Return the number of items (alias for size)
  heap.items.len

proc clear*[T](heap: var MinHeap[T]) =
  ## Remove all items
  heap.items.setLen(0)

proc toSeq*[T](heap: MinHeap[T]): seq[T] =
  ## Convert to sequence (internal array order, not sorted)
  heap.items

proc heapify*[T](items: seq[T]): MinHeap[T] =
  ## Create a min heap from a sequence
  result = MinHeap[T](items: items)
  for i in countdown((items.len - 2) div 2, 0):
    result.siftDown(i)

proc decreaseKey*[T](heap: var MinHeap[T], index: int, newValue: T) =
  ## Decrease the value at index (must be smaller than current)
  if index < 0 or index >= heap.items.len:
    raise newException(IndexDefect, "Index out of bounds")
  if newValue >= heap.items[index]:
    raise newException(ValueError, "New value must be smaller")
  heap.items[index] = newValue
  heap.siftUp(index)

when isMainModule:
  var heap = newMinHeap[int]()
  heap.insert(5)
  heap.insert(3)
  heap.insert(7)
  heap.insert(1)
  heap.insert(9)
  assert heap.peek() == 1
  assert heap.extractMin() == 1
  assert heap.extractMin() == 3
  assert heap.extractMin() == 5
  assert heap.size() == 2

  var heap2 = heapify(@[5, 3, 7, 1, 9])
  assert heap2.peek() == 1
  echo "MinHeap tests passed!"
