package datastructures

import scala.collection.mutable

/**
 * Disjoint set (Union-Find) with path compression and union by rank.
 * Time: O(α(n)) amortized for all operations (nearly constant)
 * Space: O(n)
 */
class DisjointSet[T]:
  private val parent = mutable.Map.empty[T, T]
  private val rank = mutable.Map.empty[T, Int]

  def makeSet(x: T): this.type =
    if !parent.contains(x) then
      parent(x) = x
      rank(x) = 0
    this

  def find(x: T): Option[T] =
    if !parent.contains(x) then None
    else Some(findRoot(x))

  private def findRoot(x: T): T =
    if parent(x) != x then
      parent(x) = findRoot(parent(x))
    parent(x)

  def union(x: T, y: T): Boolean =
    if !parent.contains(x) || !parent.contains(y) then false
    else
      val rootX = findRoot(x)
      val rootY = findRoot(y)

      if rootX == rootY then false
      else
        val rankX = rank(rootX)
        val rankY = rank(rootY)

        if rankX < rankY then
          parent(rootX) = rootY
        else if rankX > rankY then
          parent(rootY) = rootX
        else
          parent(rootY) = rootX
          rank(rootX) = rankX + 1
        true

  def connected(x: T, y: T): Boolean =
    (for
      rootX <- find(x)
      rootY <- find(y)
    yield rootX == rootY).getOrElse(false)

  def contains(x: T): Boolean = parent.contains(x)

  def setCount: Int =
    parent.keys.count(x => parent(x) == x)

  def setSize(x: T): Int =
    find(x) match
      case None => 0
      case Some(root) => parent.keys.count(elem => find(elem).contains(root))

  def clear(): this.type =
    parent.clear()
    rank.clear()
    this
