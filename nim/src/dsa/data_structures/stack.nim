## Stack - LIFO data structure
##
## Time Complexity:
## - push: O(1) amortized
## - pop: O(1)
## - peek: O(1)
## - isEmpty: O(1)
## - size: O(1)

type
  Stack*[T] = object
    items: seq[T]

proc newStack*[T](): Stack[T] =
  ## Create a new empty stack
  Stack[T](items: @[])

proc push*[T](s: var Stack[T], item: T) =
  ## Push an item onto the stack
  s.items.add(item)

proc pop*[T](s: var Stack[T]): T =
  ## Remove and return the top item
  ## Raises IndexDefect if stack is empty
  if s.items.len == 0:
    raise newException(IndexDefect, "Stack is empty")
  result = s.items[^1]
  s.items.setLen(s.items.len - 1)

proc peek*[T](s: Stack[T]): T =
  ## Return the top item without removing it
  ## Raises IndexDefect if stack is empty
  if s.items.len == 0:
    raise newException(IndexDefect, "Stack is empty")
  s.items[^1]

proc isEmpty*[T](s: Stack[T]): bool =
  ## Check if the stack is empty
  s.items.len == 0

proc size*[T](s: Stack[T]): int =
  ## Return the number of items in the stack
  s.items.len

proc clear*[T](s: var Stack[T]) =
  ## Remove all items from the stack
  s.items.setLen(0)

proc toSeq*[T](s: Stack[T]): seq[T] =
  ## Convert stack to sequence (bottom to top)
  s.items

when isMainModule:
  var s = newStack[int]()
  s.push(1)
  s.push(2)
  s.push(3)
  assert s.pop() == 3
  assert s.peek() == 2
  assert s.size() == 2
  assert not s.isEmpty()
  echo "Stack tests passed!"
