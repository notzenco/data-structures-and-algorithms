package algorithms

/**
 * Graph representation using adjacency list.
 */
class Graph<T>(val directed: Boolean = false) {
    private val adjacencyList = mutableMapOf<T, MutableList<T>>()

    fun addVertex(vertex: T) {
        adjacencyList.getOrPut(vertex) { mutableListOf() }
    }

    fun addEdge(from: T, to: T) {
        addVertex(from)
        addVertex(to)

        if (to !in adjacencyList[from]!!) {
            adjacencyList[from]!!.add(to)
        }

        if (!directed && from !in adjacencyList[to]!!) {
            adjacencyList[to]!!.add(from)
        }
    }

    fun removeEdge(from: T, to: T) {
        adjacencyList[from]?.remove(to)
        if (!directed) {
            adjacencyList[to]?.remove(from)
        }
    }

    fun removeVertex(vertex: T) {
        adjacencyList.remove(vertex)
        for (neighbors in adjacencyList.values) {
            neighbors.remove(vertex)
        }
    }

    fun hasEdge(from: T, to: T): Boolean = adjacencyList[from]?.contains(to) == true

    fun getNeighbors(vertex: T): List<T> = adjacencyList[vertex]?.toList() ?: emptyList()

    fun getVertices(): Set<T> = adjacencyList.keys.toSet()

    fun vertexCount(): Int = adjacencyList.size
}
