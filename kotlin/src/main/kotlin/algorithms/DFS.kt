package algorithms

import datastructures.Stack

/**
 * Depth-First Search algorithm.
 * Time: O(V + E) where V = vertices, E = edges
 * Space: O(V)
 */
object DFS {
    /**
     * Perform iterative DFS traversal from a starting vertex.
     */
    fun <T> traverse(graph: Graph<T>, start: T): List<T> {
        val result = mutableListOf<T>()
        val visited = mutableSetOf<T>()

        if (start !in graph.getVertices()) return result

        val stack = Stack<T>()
        stack.push(start)

        while (!stack.isEmpty()) {
            val vertex = stack.pop()!!

            if (vertex in visited) continue

            visited.add(vertex)
            result.add(vertex)

            // Push neighbors in reverse order to visit them in natural order
            val neighbors = graph.getNeighbors(vertex)
            for (i in neighbors.lastIndex downTo 0) {
                if (neighbors[i] !in visited) {
                    stack.push(neighbors[i])
                }
            }
        }

        return result
    }

    /**
     * Perform recursive DFS traversal from a starting vertex.
     */
    fun <T> traverseRecursive(graph: Graph<T>, start: T): List<T> {
        val result = mutableListOf<T>()
        val visited = mutableSetOf<T>()

        if (start !in graph.getVertices()) return result

        dfsRecursive(graph, start, visited, result)
        return result
    }

    private fun <T> dfsRecursive(graph: Graph<T>, vertex: T, visited: MutableSet<T>, result: MutableList<T>) {
        visited.add(vertex)
        result.add(vertex)

        for (neighbor in graph.getNeighbors(vertex)) {
            if (neighbor !in visited) {
                dfsRecursive(graph, neighbor, visited, result)
            }
        }
    }

    /**
     * Find a path between two vertices using DFS.
     */
    fun <T> findPath(graph: Graph<T>, start: T, end: T): List<T>? {
        if (start !in graph.getVertices() || end !in graph.getVertices()) return null
        if (start == end) return listOf(start)

        val visited = mutableSetOf<T>()
        val parent = mutableMapOf<T, T?>(start to null)
        val stack = Stack<T>()
        stack.push(start)

        while (!stack.isEmpty()) {
            val vertex = stack.pop()!!

            if (vertex in visited) continue
            visited.add(vertex)

            if (vertex == end) {
                return reconstructPath(parent, start, end)
            }

            for (neighbor in graph.getNeighbors(vertex)) {
                if (neighbor !in visited) {
                    parent[neighbor] = vertex
                    stack.push(neighbor)
                }
            }
        }

        return null
    }

    /**
     * Detect if the graph contains a cycle.
     */
    fun <T> hasCycle(graph: Graph<T>): Boolean {
        val visited = mutableSetOf<T>()
        val recursionStack = mutableSetOf<T>()

        for (vertex in graph.getVertices()) {
            if (vertex !in visited) {
                if (hasCycleUtil(graph, vertex, visited, recursionStack)) {
                    return true
                }
            }
        }

        return false
    }

    private fun <T> hasCycleUtil(
        graph: Graph<T>,
        vertex: T,
        visited: MutableSet<T>,
        recursionStack: MutableSet<T>
    ): Boolean {
        visited.add(vertex)
        recursionStack.add(vertex)

        for (neighbor in graph.getNeighbors(vertex)) {
            if (neighbor !in visited) {
                if (hasCycleUtil(graph, neighbor, visited, recursionStack)) {
                    return true
                }
            } else if (neighbor in recursionStack) {
                return true
            }
        }

        recursionStack.remove(vertex)
        return false
    }

    /**
     * Perform topological sort on a directed acyclic graph.
     */
    fun <T> topologicalSort(graph: Graph<T>): List<T>? {
        if (!graph.directed) return null

        val visited = mutableSetOf<T>()
        val result = mutableListOf<T>()
        val recursionStack = mutableSetOf<T>()

        for (vertex in graph.getVertices()) {
            if (vertex !in visited) {
                if (!topologicalSortUtil(graph, vertex, visited, result, recursionStack)) {
                    return null // Cycle detected
                }
            }
        }

        return result.reversed()
    }

    private fun <T> topologicalSortUtil(
        graph: Graph<T>,
        vertex: T,
        visited: MutableSet<T>,
        result: MutableList<T>,
        recursionStack: MutableSet<T>
    ): Boolean {
        visited.add(vertex)
        recursionStack.add(vertex)

        for (neighbor in graph.getNeighbors(vertex)) {
            if (neighbor in recursionStack) {
                return false // Cycle detected
            }
            if (neighbor !in visited) {
                if (!topologicalSortUtil(graph, neighbor, visited, result, recursionStack)) {
                    return false
                }
            }
        }

        recursionStack.remove(vertex)
        result.add(vertex)
        return true
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
