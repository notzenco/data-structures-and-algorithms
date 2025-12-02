package algorithms

import scala.collection.mutable

/**
 * Breadth-first search algorithms.
 * Time: O(V + E)
 * Space: O(V)
 */
object BFS:
  def traverse[T](graph: Graph[T], start: T): List[T] =
    if !graph.hasVertex(start) then return List.empty

    val visited = mutable.Set.empty[T]
    val queue = mutable.Queue[T](start)
    val result = List.newBuilder[T]

    visited += start

    while queue.nonEmpty do
      val vertex = queue.dequeue()
      result += vertex

      for neighbor <- graph.neighbors(vertex) do
        if !visited.contains(neighbor) then
          visited += neighbor
          queue.enqueue(neighbor)

    result.result()

  def shortestPath[T](graph: Graph[T], start: T, end: T): Option[List[T]] =
    if !graph.hasVertex(start) || !graph.hasVertex(end) then return None
    if start == end then return Some(List(start))

    val visited = mutable.Set.empty[T]
    val queue = mutable.Queue[T](start)
    val parent = mutable.Map.empty[T, T]

    visited += start

    while queue.nonEmpty do
      val vertex = queue.dequeue()

      if vertex == end then
        return Some(reconstructPath(parent, start, end))

      for neighbor <- graph.neighbors(vertex) do
        if !visited.contains(neighbor) then
          visited += neighbor
          parent(neighbor) = vertex
          queue.enqueue(neighbor)

    None

  private def reconstructPath[T](parent: mutable.Map[T, T], start: T, end: T): List[T] =
    val path = List.newBuilder[T]
    var current = end
    while current != start do
      path += current
      current = parent(current)
    path += start
    path.result().reverse

  def distances[T](graph: Graph[T], start: T): Map[T, Int] =
    if !graph.hasVertex(start) then return Map.empty

    val visited = mutable.Set.empty[T]
    val queue = mutable.Queue[(T, Int)]((start, 0))
    val distances = mutable.Map.empty[T, Int]

    visited += start
    distances(start) = 0

    while queue.nonEmpty do
      val (vertex, dist) = queue.dequeue()

      for neighbor <- graph.neighbors(vertex) do
        if !visited.contains(neighbor) then
          visited += neighbor
          distances(neighbor) = dist + 1
          queue.enqueue((neighbor, dist + 1))

    distances.toMap
