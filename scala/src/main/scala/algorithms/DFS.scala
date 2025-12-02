package algorithms

import scala.collection.mutable

/**
 * Depth-first search algorithms.
 * Time: O(V + E)
 * Space: O(V)
 */
object DFS:
  def traverse[T](graph: Graph[T], start: T): List[T] =
    if !graph.hasVertex(start) then return List.empty

    val visited = mutable.Set.empty[T]
    val stack = mutable.Stack[T](start)
    val result = List.newBuilder[T]

    while stack.nonEmpty do
      val vertex = stack.pop()
      if !visited.contains(vertex) then
        visited += vertex
        result += vertex

        for neighbor <- graph.neighbors(vertex) do
          if !visited.contains(neighbor) then
            stack.push(neighbor)

    result.result()

  def traverseRecursive[T](graph: Graph[T], start: T): List[T] =
    if !graph.hasVertex(start) then return List.empty

    val visited = mutable.Set.empty[T]
    val result = List.newBuilder[T]

    def dfs(vertex: T): Unit =
      visited += vertex
      result += vertex
      for neighbor <- graph.neighbors(vertex) do
        if !visited.contains(neighbor) then
          dfs(neighbor)

    dfs(start)
    result.result()

  def findPath[T](graph: Graph[T], start: T, end: T): Option[List[T]] =
    if !graph.hasVertex(start) || !graph.hasVertex(end) then return None
    if start == end then return Some(List(start))

    val visited = mutable.Set.empty[T]
    val parent = mutable.Map.empty[T, T]
    val stack = mutable.Stack[T](start)

    while stack.nonEmpty do
      val vertex = stack.pop()
      if !visited.contains(vertex) then
        visited += vertex

        if vertex == end then
          return Some(reconstructPath(parent, start, end))

        for neighbor <- graph.neighbors(vertex) do
          if !visited.contains(neighbor) then
            parent(neighbor) = vertex
            stack.push(neighbor)

    None

  private def reconstructPath[T](parent: mutable.Map[T, T], start: T, end: T): List[T] =
    val path = List.newBuilder[T]
    var current = end
    while current != start do
      path += current
      current = parent(current)
    path += start
    path.result().reverse

  def hasCycle[T](graph: Graph[T]): Boolean =
    val visited = mutable.Set.empty[T]
    val recStack = mutable.Set.empty[T]

    def hasCycleUtil(vertex: T): Boolean =
      visited += vertex
      recStack += vertex

      for neighbor <- graph.neighbors(vertex) do
        if !visited.contains(neighbor) then
          if hasCycleUtil(neighbor) then return true
        else if recStack.contains(neighbor) then
          return true

      recStack -= vertex
      false

    for vertex <- graph.vertices do
      if !visited.contains(vertex) then
        if hasCycleUtil(vertex) then return true

    false

  def topologicalSort[T](graph: Graph[T]): Option[List[T]] =
    if !graph.directed then return None

    val visited = mutable.Set.empty[T]
    val recStack = mutable.Set.empty[T]
    val result = mutable.Stack.empty[T]

    def visit(vertex: T): Boolean =
      if recStack.contains(vertex) then return false
      if visited.contains(vertex) then return true

      visited += vertex
      recStack += vertex

      for neighbor <- graph.neighbors(vertex) do
        if !visit(neighbor) then return false

      recStack -= vertex
      result.push(vertex)
      true

    for vertex <- graph.vertices do
      if !visited.contains(vertex) then
        if !visit(vertex) then return None

    Some(result.toList)
