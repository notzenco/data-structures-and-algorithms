## Breadth-First Search - Level-order graph traversal
##
## Time Complexity: O(V + E)
## Space Complexity: O(V)

import std/tables
import std/sets
import std/deques
import graph

proc traverse*[T](g: Graph[T], start: T): seq[T] =
  ## Traverse graph starting from vertex using BFS
  ## Returns vertices in BFS order
  result = @[]
  if not g.hasVertex(start):
    return result

  var visited = initHashSet[T]()
  var queue = initDeque[T]()

  queue.addLast(start)
  visited.incl(start)

  while queue.len > 0:
    let vertex = queue.popFirst()
    result.add(vertex)

    for neighbor in g.neighbors(vertex):
      if neighbor notin visited:
        visited.incl(neighbor)
        queue.addLast(neighbor)

proc shortestPath*[T](g: Graph[T], start, target: T): seq[T] =
  ## Find shortest path between start and target
  ## Returns empty sequence if no path exists
  if not g.hasVertex(start) or not g.hasVertex(target):
    return @[]

  if start == target:
    return @[start]

  var visited = initHashSet[T]()
  var parent = initTable[T, T]()
  var queue = initDeque[T]()

  queue.addLast(start)
  visited.incl(start)

  while queue.len > 0:
    let vertex = queue.popFirst()

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
        visited.incl(neighbor)
        parent[neighbor] = vertex
        queue.addLast(neighbor)

  @[] # No path found

proc distances*[T](g: Graph[T], start: T): Table[T, int] =
  ## Calculate distances from start to all reachable vertices
  ## Returns table mapping vertex to distance
  result = initTable[T, int]()
  if not g.hasVertex(start):
    return result

  var queue = initDeque[T]()
  queue.addLast(start)
  result[start] = 0

  while queue.len > 0:
    let vertex = queue.popFirst()
    let dist = result[vertex]

    for neighbor in g.neighbors(vertex):
      if not result.hasKey(neighbor):
        result[neighbor] = dist + 1
        queue.addLast(neighbor)

proc pathExists*[T](g: Graph[T], start, target: T): bool =
  ## Check if path exists between start and target
  if not g.hasVertex(start) or not g.hasVertex(target):
    return false

  if start == target:
    return true

  var visited = initHashSet[T]()
  var queue = initDeque[T]()

  queue.addLast(start)
  visited.incl(start)

  while queue.len > 0:
    let vertex = queue.popFirst()

    for neighbor in g.neighbors(vertex):
      if neighbor == target:
        return true
      if neighbor notin visited:
        visited.incl(neighbor)
        queue.addLast(neighbor)

  false

proc connectedComponents*[T](g: Graph[T]): seq[seq[T]] =
  ## Find all connected components in undirected graph
  result = @[]
  var visited = initHashSet[T]()

  for vertex in g.vertices():
    if vertex notin visited:
      var component: seq[T] = @[]
      var queue = initDeque[T]()

      queue.addLast(vertex)
      visited.incl(vertex)

      while queue.len > 0:
        let v = queue.popFirst()
        component.add(v)

        for neighbor in g.neighbors(v):
          if neighbor notin visited:
            visited.incl(neighbor)
            queue.addLast(neighbor)

      result.add(component)

proc isConnected*[T](g: Graph[T]): bool =
  ## Check if graph is connected (all vertices reachable from any vertex)
  if g.isEmpty():
    return true

  let allVertices = g.vertices()
  if allVertices.len == 0:
    return true

  let traversed = traverse(g, allVertices[0])
  traversed.len == g.vertexCount()

when isMainModule:
  var g = newGraph[int](directed = false)
  g.addEdge(1, 2)
  g.addEdge(1, 3)
  g.addEdge(2, 4)
  g.addEdge(3, 4)
  g.addEdge(4, 5)

  let bfsOrder = traverse(g, 1)
  assert bfsOrder[0] == 1
  assert bfsOrder.len == 5

  let path = shortestPath(g, 1, 5)
  assert path.len == 3  # 1 -> 2/3 -> 4 -> 5
  assert path[0] == 1
  assert path[^1] == 5

  let dists = distances(g, 1)
  assert dists[1] == 0
  assert dists[5] == 3

  assert pathExists(g, 1, 5)
  assert isConnected(g)

  echo "BFS tests passed!"
