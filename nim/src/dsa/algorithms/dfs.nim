## Depth-First Search - Graph traversal algorithm
##
## Time Complexity: O(V + E)
## Space Complexity: O(V)

import std/tables
import std/sets
import graph

proc traverse*[T](g: Graph[T], start: T): seq[T] =
  ## Traverse graph starting from vertex using DFS (iterative)
  ## Returns vertices in DFS order
  result = @[]
  if not g.hasVertex(start):
    return result

  var visited = initHashSet[T]()
  var stack: seq[T] = @[start]

  while stack.len > 0:
    let vertex = stack.pop()

    if vertex in visited:
      continue

    visited.incl(vertex)
    result.add(vertex)

    for neighbor in g.neighbors(vertex):
      if neighbor notin visited:
        stack.add(neighbor)

proc traverseRecursive*[T](g: Graph[T], start: T): seq[T] =
  ## Traverse graph using recursive DFS
  result = @[]
  if not g.hasVertex(start):
    return result

  var visited = initHashSet[T]()

  proc dfs(vertex: T) =
    visited.incl(vertex)
    result.add(vertex)
    for neighbor in g.neighbors(vertex):
      if neighbor notin visited:
        dfs(neighbor)

  dfs(start)

proc findPath*[T](g: Graph[T], start, target: T): seq[T] =
  ## Find a path from start to target using DFS
  ## Returns empty sequence if no path exists
  if not g.hasVertex(start) or not g.hasVertex(target):
    return @[]

  if start == target:
    return @[start]

  var visited = initHashSet[T]()
  var parent = initTable[T, T]()
  var stack: seq[T] = @[start]

  while stack.len > 0:
    let vertex = stack.pop()

    if vertex in visited:
      continue

    visited.incl(vertex)

    if vertex == target:
      # Reconstruct path
      result = @[]
      var current = target
      while current != start:
        result.insert(current, 0)
        current = parent[current]
      result.insert(start, 0)
      return result

    for neighbor in g.neighbors(vertex):
      if neighbor notin visited:
        parent[neighbor] = vertex
        stack.add(neighbor)

  @[] # No path found

proc hasCycle*[T](g: Graph[T]): bool =
  ## Check if directed graph has a cycle
  if not g.isDirected():
    # For undirected graphs, use different logic
    var visited = initHashSet[T]()

    proc hasCycleUndirected(vertex: T, parent: T): bool =
      visited.incl(vertex)
      for neighbor in g.neighbors(vertex):
        if neighbor notin visited:
          if hasCycleUndirected(neighbor, vertex):
            return true
        elif neighbor != parent:
          return true
      false

    for v in g.vertices():
      if v notin visited:
        if hasCycleUndirected(v, v):
          return true
    return false

  # For directed graphs
  var visited = initHashSet[T]()
  var recStack = initHashSet[T]()

  proc hasCycleUtil(vertex: T): bool =
    visited.incl(vertex)
    recStack.incl(vertex)

    for neighbor in g.neighbors(vertex):
      if neighbor notin visited:
        if hasCycleUtil(neighbor):
          return true
      elif neighbor in recStack:
        return true

    recStack.excl(vertex)
    false

  for v in g.vertices():
    if v notin visited:
      if hasCycleUtil(v):
        return true

  false

proc topologicalSort*[T](g: Graph[T]): seq[T] =
  ## Topological sort for directed acyclic graph
  ## Returns empty sequence if graph has a cycle
  if not g.isDirected():
    raise newException(ValueError, "Topological sort requires directed graph")

  var visited = initHashSet[T]()
  var recStack = initHashSet[T]()
  var sorted: seq[T] = @[]
  var hasCycle = false

  proc visit(vertex: T) =
    if hasCycle:
      return
    if vertex in recStack:
      hasCycle = true
      return
    if vertex in visited:
      return

    recStack.incl(vertex)
    for neighbor in g.neighbors(vertex):
      visit(neighbor)
    recStack.excl(vertex)
    visited.incl(vertex)
    sorted.insert(vertex, 0)

  for v in g.vertices():
    if v notin visited:
      visit(v)

  if hasCycle:
    return @[]
  sorted

proc allPaths*[T](g: Graph[T], start, target: T): seq[seq[T]] =
  ## Find all paths from start to target
  result = @[]
  if not g.hasVertex(start) or not g.hasVertex(target):
    return result

  var path: seq[T] = @[]
  var visited = initHashSet[T]()

  proc findAllPaths(current: T) =
    visited.incl(current)
    path.add(current)

    if current == target:
      result.add(path)
    else:
      for neighbor in g.neighbors(current):
        if neighbor notin visited:
          findAllPaths(neighbor)

    discard path.pop()
    visited.excl(current)

  findAllPaths(start)

when isMainModule:
  var g = newGraph[int](directed = false)
  g.addEdge(1, 2)
  g.addEdge(1, 3)
  g.addEdge(2, 4)
  g.addEdge(3, 4)
  g.addEdge(4, 5)

  let dfsOrder = traverse(g, 1)
  assert dfsOrder[0] == 1
  assert dfsOrder.len == 5

  let path = findPath(g, 1, 5)
  assert path.len >= 3
  assert path[0] == 1
  assert path[^1] == 5

  # Test cycle detection
  var cyclic = newGraph[int](directed = true)
  cyclic.addEdge(1, 2)
  cyclic.addEdge(2, 3)
  cyclic.addEdge(3, 1)
  assert hasCycle(cyclic)

  var acyclic = newGraph[int](directed = true)
  acyclic.addEdge(1, 2)
  acyclic.addEdge(2, 3)
  acyclic.addEdge(1, 3)
  assert not hasCycle(acyclic)

  # Test topological sort
  let sorted = topologicalSort(acyclic)
  assert sorted.len == 3
  assert sorted[0] == 1

  echo "DFS tests passed!"
