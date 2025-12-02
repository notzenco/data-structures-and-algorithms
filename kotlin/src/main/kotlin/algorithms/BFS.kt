package algorithms

import datastructures.Queue

/**
 * Breadth-First Search algorithm.
 * Time: O(V + E) where V = vertices, E = edges
 * Space: O(V)
 */
object BFS {
    /**
     * Perform BFS traversal from a starting vertex.
     */
    fun <T> traverse(graph: Graph<T>, start: T): List<T> {
        val result = mutableListOf<T>()
        val visited = mutableSetOf<T>()

        if (start !in graph.getVertices()) return result

        val queue = Queue<T>()
        queue.enqueue(start)
        visited.add(start)

        while (!queue.isEmpty()) {
            val vertex = queue.dequeue()!!
            result.add(vertex)

            for (neighbor in graph.getNeighbors(vertex)) {
                if (neighbor !in visited) {
                    visited.add(neighbor)
                    queue.enqueue(neighbor)
                }
            }
        }

        return result
    }

    /**
     * Find the shortest path between two vertices using BFS.
     * Only works for unweighted graphs.
     */
    fun <T> shortestPath(graph: Graph<T>, start: T, end: T): List<T>? {
        if (start !in graph.getVertices() || end !in graph.getVertices()) return null
        if (start == end) return listOf(start)

        val queue = Queue<T>()
        queue.enqueue(start)
        val visited = mutableSetOf(start)
        val parent = mutableMapOf<T, T?>(start to null)

        while (!queue.isEmpty()) {
            val vertex = queue.dequeue()!!

            for (neighbor in graph.getNeighbors(vertex)) {
                if (neighbor !in visited) {
                    visited.add(neighbor)
                    parent[neighbor] = vertex

                    if (neighbor == end) {
                        return reconstructPath(parent, start, end)
                    }

                    queue.enqueue(neighbor)
                }
            }
        }

        return null
    }

    /**
     * Find distances from start vertex to all reachable vertices.
     */
    fun <T> distances(graph: Graph<T>, start: T): Map<T, Int> {
        val distances = mutableMapOf<T, Int>()

        if (start !in graph.getVertices()) return distances

        val queue = Queue<T>()
        queue.enqueue(start)
        distances[start] = 0

        while (!queue.isEmpty()) {
            val vertex = queue.dequeue()!!
            val currentDistance = distances[vertex]!!

            for (neighbor in graph.getNeighbors(vertex)) {
                if (neighbor !in distances) {
                    distances[neighbor] = currentDistance + 1
                    queue.enqueue(neighbor)
                }
            }
        }

        return distances
    }

    private fun <T> reconstructPath(parent: Map<T, T?>, start: T, end: T): List<T> {
        val path = mutableListOf<T>()
        var current: T? = end

        while (current != null) {
            path.add(0, current)
            current = parent[current]
        }

        return path
    }
}
