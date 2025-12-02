package algorithms

import scala.collection.mutable

/**
 * Graph implementation using adjacency list.
 * Supports both directed and undirected graphs.
 */
class Graph[T](val directed: Boolean = false):
  private val adjacencyList = mutable.Map.empty[T, mutable.Set[T]]

  def addVertex(vertex: T): this.type =
    if !adjacencyList.contains(vertex) then
      adjacencyList(vertex) = mutable.Set.empty[T]
    this

  def addEdge(from: T, to: T): this.type =
    addVertex(from)
    addVertex(to)
    adjacencyList(from) += to
    if !directed then adjacencyList(to) += from
    this

  def removeVertex(vertex: T): this.type =
    adjacencyList.remove(vertex)
    for neighbors <- adjacencyList.values do
      neighbors -= vertex
    this

  def removeEdge(from: T, to: T): this.type =
    adjacencyList.get(from).foreach(_ -= to)
    if !directed then adjacencyList.get(to).foreach(_ -= from)
    this

  def hasVertex(vertex: T): Boolean = adjacencyList.contains(vertex)

  def hasEdge(from: T, to: T): Boolean =
    adjacencyList.get(from).exists(_.contains(to))

  def neighbors(vertex: T): Set[T] =
    adjacencyList.get(vertex).map(_.toSet).getOrElse(Set.empty)

  def vertices: Set[T] = adjacencyList.keySet.toSet

  def vertexCount: Int = adjacencyList.size

  def edgeCount: Int =
    val count = adjacencyList.values.map(_.size).sum
    if directed then count else count / 2

  def clear(): this.type =
    adjacencyList.clear()
    this
