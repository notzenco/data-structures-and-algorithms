## Dynamic Array - Resizable array implementation
##
## Time Complexity:
## - get/set: O(1)
## - push: O(1) amortized
## - pop: O(1)
## - insert: O(n)
## - removeAt: O(n)

type
  DynamicArray*[T] = object
    items: seq[T]

proc newDynamicArray*[T](): DynamicArray[T] =
  ## Create a new empty dynamic array
  DynamicArray[T](items: @[])

proc newDynamicArray*[T](capacity: int): DynamicArray[T] =
  ## Create a new dynamic array with initial capacity
  result = DynamicArray[T](items: @[])
  result.items.setLen(0)
  result.items = newSeqOfCap[T](capacity)

proc push*[T](arr: var DynamicArray[T], item: T) =
  ## Add an item to the end
  arr.items.add(item)

proc pop*[T](arr: var DynamicArray[T]): T =
  ## Remove and return the last item
  ## Raises IndexDefect if array is empty
  if arr.items.len == 0:
    raise newException(IndexDefect, "Array is empty")
  result = arr.items[^1]
  arr.items.setLen(arr.items.len - 1)

proc get*[T](arr: DynamicArray[T], index: int): T =
  ## Get item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= arr.items.len:
    raise newException(IndexDefect, "Index out of bounds")
  arr.items[index]

proc set*[T](arr: var DynamicArray[T], index: int, item: T) =
  ## Set item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= arr.items.len:
    raise newException(IndexDefect, "Index out of bounds")
  arr.items[index] = item

proc `[]`*[T](arr: DynamicArray[T], index: int): T =
  ## Get item at index using [] operator
  arr.get(index)

proc `[]=`*[T](arr: var DynamicArray[T], index: int, item: T) =
  ## Set item at index using []= operator
  arr.set(index, item)

proc insert*[T](arr: var DynamicArray[T], index: int, item: T) =
  ## Insert item at index, shifting subsequent items right
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index > arr.items.len:
    raise newException(IndexDefect, "Index out of bounds")
  arr.items.insert(item, index)

proc removeAt*[T](arr: var DynamicArray[T], index: int): T =
  ## Remove and return item at index
  ## Raises IndexDefect if index is out of bounds
  if index < 0 or index >= arr.items.len:
    raise newException(IndexDefect, "Index out of bounds")
  result = arr.items[index]
  arr.items.delete(index)

proc find*[T](arr: DynamicArray[T], item: T): int =
  ## Find index of item, returns -1 if not found
  for i, x in arr.items:
    if x == item:
      return i
  -1

proc contains*[T](arr: DynamicArray[T], item: T): bool =
  ## Check if array contains item
  arr.find(item) != -1

proc isEmpty*[T](arr: DynamicArray[T]): bool =
  ## Check if the array is empty
  arr.items.len == 0

proc size*[T](arr: DynamicArray[T]): int =
  ## Return the number of items
  arr.items.len

proc len*[T](arr: DynamicArray[T]): int =
  ## Return the number of items (alias for size)
  arr.items.len

proc clear*[T](arr: var DynamicArray[T]) =
  ## Remove all items
  arr.items.setLen(0)

proc toSeq*[T](arr: DynamicArray[T]): seq[T] =
  ## Convert to sequence
  arr.items

iterator items*[T](arr: DynamicArray[T]): T =
  ## Iterate over items
  for item in arr.items:
    yield item

when isMainModule:
  var arr = newDynamicArray[int]()
  arr.push(1)
  arr.push(2)
  arr.push(3)
  assert arr[0] == 1
  assert arr[1] == 2
  arr[1] = 5
  assert arr[1] == 5
  assert arr.pop() == 3
  assert arr.size() == 2
  arr.insert(1, 10)
  assert arr[1] == 10
  assert arr.removeAt(1) == 10
  echo "DynamicArray tests passed!"
