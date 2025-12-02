package datastructures

/**
 * Disjoint Set (Union-Find) implementation with path compression and union by rank.
 * Time: O(α(n)) amortized for all operations (nearly constant)
 * Space: O(n)
 */
class DisjointSet(size: Int) {
    private val parent = IntArray(size) { it }
    private val rank = IntArray(size) { 0 }
    private var setCount = size

    fun find(x: Int): Int {
        if (parent[x] != x) {
            parent[x] = find(parent[x]) // Path compression
        }
        return parent[x]
    }

    fun union(x: Int, y: Int): Boolean {
        val rootX = find(x)
        val rootY = find(y)

        if (rootX == rootY) return false

        // Union by rank
        when {
            rank[rootX] < rank[rootY] -> parent[rootX] = rootY
            rank[rootX] > rank[rootY] -> parent[rootY] = rootX
            else -> {
                parent[rootY] = rootX
                rank[rootX]++
            }
        }

        setCount--
        return true
    }

    fun connected(x: Int, y: Int): Boolean = find(x) == find(y)

    fun count(): Int = setCount

    fun size(): Int = parent.size
}
