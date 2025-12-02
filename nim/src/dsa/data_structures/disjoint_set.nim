## Disjoint Set (Union-Find) with path compression and union by rank
##
## Time Complexity:
## - makeSet: O(1)
## - find: O(α(n)) amortized (nearly constant)
## - union: O(α(n)) amortized (nearly constant)
## - connected: O(α(n)) amortized (nearly constant)

import std/tables

type
  DisjointSet*[T] = object
    parent: Table[T, T]
    rank: Table[T, int]

proc newDisjointSet*[T](): DisjointSet[T] =
  ## Create a new empty disjoint set
  DisjointSet[T](parent: initTable[T, T](), rank: initTable[T, int]())

proc makeSet*[T](ds: var DisjointSet[T], x: T) =
  ## Create a new set containing only x
  if not ds.parent.hasKey(x):
    ds.parent[x] = x
    ds.rank[x] = 0

proc find*[T](ds: var DisjointSet[T], x: T): T =
  ## Find the representative of the set containing x
  ## Raises KeyError if x is not in any set
  if not ds.parent.hasKey(x):
    raise newException(KeyError, "Element not in any set")

  if ds.parent[x] != x:
    # Path compression
    ds.parent[x] = ds.find(ds.parent[x])
  ds.parent[x]

proc union*[T](ds: var DisjointSet[T], x, y: T): bool =
  ## Merge the sets containing x and y
  ## Returns true if sets were different (and thus merged)
  let rootX = ds.find(x)
  let rootY = ds.find(y)

  if rootX == rootY:
    return false

  # Union by rank
  let rankX = ds.rank[rootX]
  let rankY = ds.rank[rootY]

  if rankX < rankY:
    ds.parent[rootX] = rootY
  elif rankX > rankY:
    ds.parent[rootY] = rootX
  else:
    ds.parent[rootY] = rootX
    ds.rank[rootX] = rankX + 1

  true

proc unionSets*[T](ds: var DisjointSet[T], x, y: T): bool =
  ## Merge the sets containing x and y (alias for union)
  ds.union(x, y)

proc connected*[T](ds: var DisjointSet[T], x, y: T): bool =
  ## Check if x and y are in the same set
  ds.find(x) == ds.find(y)

proc contains*[T](ds: DisjointSet[T], x: T): bool =
  ## Check if x is in any set
  ds.parent.hasKey(x)

proc setCount*[T](ds: var DisjointSet[T]): int =
  ## Count the number of disjoint sets
  var roots: seq[T] = @[]
  for x in ds.parent.keys:
    let root = ds.find(x)
    if root notin roots:
      roots.add(root)
  roots.len

proc size*[T](ds: DisjointSet[T]): int =
  ## Return total number of elements
  ds.parent.len

proc len*[T](ds: DisjointSet[T]): int =
  ## Return total number of elements (alias for size)
  ds.parent.len

proc clear*[T](ds: var DisjointSet[T]) =
  ## Remove all sets
  ds.parent.clear()
  ds.rank.clear()

proc getSetMembers*[T](ds: var DisjointSet[T], x: T): seq[T] =
  ## Get all members of the set containing x
  let root = ds.find(x)
  result = @[]
  for element in ds.parent.keys:
    if ds.find(element) == root:
      result.add(element)

when isMainModule:
  var ds = newDisjointSet[int]()
  ds.makeSet(1)
  ds.makeSet(2)
  ds.makeSet(3)
  ds.makeSet(4)
  ds.makeSet(5)

  assert not ds.connected(1, 2)
  discard ds.union(1, 2)
  assert ds.connected(1, 2)

  discard ds.union(3, 4)
  assert ds.connected(3, 4)
  assert not ds.connected(1, 3)

  discard ds.union(2, 3)
  assert ds.connected(1, 4)

  assert ds.setCount() == 2 # {1,2,3,4} and {5}
  echo "DisjointSet tests passed!"
