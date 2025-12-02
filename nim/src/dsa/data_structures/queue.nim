## Queue - FIFO data structure
##
## Time Complexity:
## - enqueue: O(1) amortized
## - dequeue: O(1) amortized
## - peek: O(1)
## - isEmpty: O(1)
## - size: O(1)

type
  Queue*[T] = object
    items: seq[T]
    front: int

proc newQueue*[T](): Queue[T] =
  ## Create a new empty queue
  Queue[T](items: @[], front: 0)

proc enqueue*[T](q: var Queue[T], item: T) =
  ## Add an item to the back of the queue
  q.items.add(item)

proc dequeue*[T](q: var Queue[T]): T =
  ## Remove and return the front item
  ## Raises IndexDefect if queue is empty
  if q.front >= q.items.len:
    raise newException(IndexDefect, "Queue is empty")
  result = q.items[q.front]
  inc q.front
  # Compact if too much wasted space
  if q.front > q.items.len div 2 and q.front > 16:
    q.items = q.items[q.front .. ^1]
    q.front = 0

proc peek*[T](q: Queue[T]): T =
  ## Return the front item without removing it
  ## Raises IndexDefect if queue is empty
  if q.front >= q.items.len:
    raise newException(IndexDefect, "Queue is empty")
  q.items[q.front]

proc isEmpty*[T](q: Queue[T]): bool =
  ## Check if the queue is empty
  q.front >= q.items.len

proc size*[T](q: Queue[T]): int =
  ## Return the number of items in the queue
  q.items.len - q.front

proc clear*[T](q: var Queue[T]) =
  ## Remove all items from the queue
  q.items.setLen(0)
  q.front = 0

proc toSeq*[T](q: Queue[T]): seq[T] =
  ## Convert queue to sequence (front to back)
  if q.front >= q.items.len:
    @[]
  else:
    q.items[q.front .. ^1]

when isMainModule:
  var q = newQueue[int]()
  q.enqueue(1)
  q.enqueue(2)
  q.enqueue(3)
  assert q.dequeue() == 1
  assert q.peek() == 2
  assert q.size() == 2
  assert not q.isEmpty()
  echo "Queue tests passed!"
