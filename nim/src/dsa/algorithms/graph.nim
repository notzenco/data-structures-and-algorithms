## Graph - Adjacency list representation
##
## Supports both directed and undirected graphs

import std/tables
import std/sets

type
  Graph*[T] = object
    adjacencyList: Table[T, HashSet[T]]
    directed: bool

proc newGraph*[T](directed: bool = false): Graph[T] =
  ## Create a new graph
  Graph[T](adjacencyList: initTable[T, HashSet[T]](), directed: directed)

proc addVertex*[T](g: var Graph[T], vertex: T) =
  ## Add a vertex to the graph
  if not g.adjacencyList.hasKey(vertex):
    g.adjacencyList[vertex] = initHashSet[T]()

proc addEdge*[T](g: var Graph[T], fromVertex, toVertex: T) =
  ## Add an edge between two vertices
  g.addVertex(fromVertex)
  g.addVertex(toVertex)
  g.adjacencyList[fromVertex].incl(toVertex)
  if not g.directed:
    g.adjacencyList[toVertex].incl(fromVertex)

proc removeEdge*[T](g: var Graph[T], fromVertex, toVertex: T): bool =
  ## Remove an edge between two vertices
  ## Returns true if edge existed
  if not g.adjacencyList.hasKey(fromVertex):
    return false
  if toVertex notin g.adjacencyList[fromVertex]:
    return false
  g.adjacencyList[fromVertex].excl(toVertex)
  if not g.directed and g.adjacencyList.hasKey(toVertex):
    g.adjacencyList[toVertex].excl(fromVertex)
  true

proc removeVertex*[T](g: var Graph[T], vertex: T): bool =
  ## Remove a vertex and all its edges
  ## Returns true if vertex existed
  if not g.adjacencyList.hasKey(vertex):
    return false
  g.adjacencyList.del(vertex)
  for v in g.adjacencyList.keys:
    g.adjacencyList[v].excl(vertex)
  true

proc hasVertex*[T](g: Graph[T], vertex: T): bool =
  ## Check if vertex exists
  g.adjacencyList.hasKey(vertex)

proc hasEdge*[T](g: Graph[T], fromVertex, toVertex: T): bool =
  ## Check if edge exists
  if not g.adjacencyList.hasKey(fromVertex):
    return false
  toVertex in g.adjacencyList[fromVertex]

proc neighbors*[T](g: Graph[T], vertex: T): seq[T] =
  ## Get all neighbors of a vertex
  if not g.adjacencyList.hasKey(vertex):
    return @[]
  result = @[]
  for neighbor in g.adjacencyList[vertex]:
    result.add(neighbor)

proc vertices*[T](g: Graph[T]): seq[T] =
  ## Get all vertices
  result = @[]
  for v in g.adjacencyList.keys:
    result.add(v)

proc vertexCount*[T](g: Graph[T]): int =
  ## Get number of vertices
  g.adjacencyList.len

proc edgeCount*[T](g: Graph[T]): int =
  ## Get number of edges
  var count = 0
  for neighbors in g.adjacencyList.values:
    count += neighbors.len
  if not g.directed:
    count = count div 2
  count

proc isDirected*[T](g: Graph[T]): bool =
  ## Check if graph is directed
  g.directed

proc degree*[T](g: Graph[T], vertex: T): int =
  ## Get degree of a vertex
  if not g.adjacencyList.hasKey(vertex):
    return 0
  g.adjacencyList[vertex].len

proc isEmpty*[T](g: Graph[T]): bool =
  ## Check if graph is empty
  g.adjacencyList.len == 0

proc clear*[T](g: var Graph[T]) =
  ## Remove all vertices and edges
  g.adjacencyList.clear()

iterator vertexIterator*[T](g: Graph[T]): T =
  ## Iterate over vertices
  for v in g.adjacencyList.keys:
    yield v

iterator edgeIterator*[T](g: Graph[T]): (T, T) =
  ## Iterate over edges
  var seen = initHashSet[(T, T)]()
  for fromV in g.adjacencyList.keys:
    for toV in g.adjacencyList[fromV]:
      if g.directed or (toV, fromV) notin seen:
        yield (fromV, toV)
        seen.incl((fromV, toV))

when isMainModule:
  var g = newGraph[int](directed = false)
  g.addEdge(1, 2)
  g.addEdge(1, 3)
  g.addEdge(2, 4)

  assert g.hasVertex(1)
  assert g.hasVertex(2)
  assert g.hasEdge(1, 2)
  assert g.hasEdge(2, 1)  # Undirected
  assert g.vertexCount() == 4
  assert g.edgeCount() == 3
  assert g.degree(1) == 2

  var dg = newGraph[int](directed = true)
  dg.addEdge(1, 2)
  dg.addEdge(1, 3)
  assert dg.hasEdge(1, 2)
  assert not dg.hasEdge(2, 1)  # Directed
  echo "Graph tests passed!"
