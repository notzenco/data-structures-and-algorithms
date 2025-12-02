import algorithms.*
import kotlin.test.*

class AlgorithmsTest {
    // Binary Search Tests
    @Test
    fun `binary search`() {
        val arr = listOf(1, 3, 5, 7, 9, 11, 13)

        assertEquals(0, BinarySearch.search(arr, 1))
        assertEquals(3, BinarySearch.search(arr, 7))
        assertEquals(6, BinarySearch.search(arr, 13))
        assertEquals(-1, BinarySearch.search(arr, 4))
        assertEquals(-1, BinarySearch.search(arr, 0))
        assertEquals(-1, BinarySearch.search(arr, 14))
    }

    @Test
    fun `binary search lower bound`() {
        val arr = listOf(1, 3, 5, 7, 9)

        assertEquals(0, BinarySearch.lowerBound(arr, 1))
        assertEquals(2, BinarySearch.lowerBound(arr, 5))
        assertEquals(2, BinarySearch.lowerBound(arr, 4)) // Between 3 and 5
        assertEquals(0, BinarySearch.lowerBound(arr, 0)) // Before all
        assertEquals(5, BinarySearch.lowerBound(arr, 10)) // After all
    }

    @Test
    fun `binary search upper bound`() {
        val arr = listOf(1, 3, 5, 7, 9)

        assertEquals(1, BinarySearch.upperBound(arr, 1))
        assertEquals(3, BinarySearch.upperBound(arr, 5))
        assertEquals(2, BinarySearch.upperBound(arr, 4)) // Between 3 and 5
        assertEquals(0, BinarySearch.upperBound(arr, 0)) // Before all
        assertEquals(5, BinarySearch.upperBound(arr, 10)) // After all
    }

    // Insertion Sort Tests
    @Test
    fun `insertion sort`() {
        val arr = mutableListOf(64, 34, 25, 12, 22, 11, 90)
        InsertionSort.sort(arr)
        assertEquals(listOf(11, 12, 22, 25, 34, 64, 90), arr)
    }

    @Test
    fun `insertion sort sorted`() {
        val arr = listOf(5, 2, 8, 1, 9)
        val sorted = InsertionSort.sorted(arr)
        assertEquals(listOf(1, 2, 5, 8, 9), sorted)
        assertEquals(listOf(5, 2, 8, 1, 9), arr) // Original unchanged
    }

    @Test
    fun `insertion sort descending`() {
        val arr = mutableListOf(3, 1, 4, 1, 5)
        InsertionSort.sort(arr, compareByDescending { it })
        assertEquals(listOf(5, 4, 3, 1, 1), arr)
    }

    // Merge Sort Tests
    @Test
    fun `merge sort`() {
        val arr = mutableListOf(64, 34, 25, 12, 22, 11, 90)
        MergeSort.sort(arr)
        assertEquals(listOf(11, 12, 22, 25, 34, 64, 90), arr)
    }

    @Test
    fun `merge sort sorted`() {
        val arr = listOf(5, 2, 8, 1, 9)
        val sorted = MergeSort.sorted(arr)
        assertEquals(listOf(1, 2, 5, 8, 9), sorted)
    }

    @Test
    fun `merge sort empty`() {
        val arr = mutableListOf<Int>()
        MergeSort.sort(arr)
        assertEquals(emptyList<Int>(), arr)
    }

    @Test
    fun `merge sort single element`() {
        val arr = mutableListOf(42)
        MergeSort.sort(arr)
        assertEquals(listOf(42), arr)
    }

    // Quick Sort Tests
    @Test
    fun `quick sort`() {
        val arr = mutableListOf(64, 34, 25, 12, 22, 11, 90)
        QuickSort.sort(arr)
        assertEquals(listOf(11, 12, 22, 25, 34, 64, 90), arr)
    }

    @Test
    fun `quick sort sorted`() {
        val arr = listOf(5, 2, 8, 1, 9)
        val sorted = QuickSort.sorted(arr)
        assertEquals(listOf(1, 2, 5, 8, 9), sorted)
    }

    @Test
    fun `quick sort with duplicates`() {
        val arr = mutableListOf(3, 1, 4, 1, 5, 9, 2, 6, 5, 3)
        QuickSort.sort(arr)
        assertEquals(listOf(1, 1, 2, 3, 3, 4, 5, 5, 6, 9), arr)
    }

    // Graph Tests
    @Test
    fun `graph operations`() {
        val graph = Graph<Int>()

        graph.addEdge(1, 2)
        graph.addEdge(1, 3)
        graph.addEdge(2, 4)

        assertTrue(graph.hasEdge(1, 2))
        assertTrue(graph.hasEdge(2, 1)) // Undirected
        assertFalse(graph.hasEdge(1, 4))

        assertEquals(listOf(2, 3), graph.getNeighbors(1))

        graph.removeEdge(1, 2)
        assertFalse(graph.hasEdge(1, 2))
    }

    @Test
    fun `directed graph`() {
        val graph = Graph<Int>(directed = true)

        graph.addEdge(1, 2)
        graph.addEdge(2, 3)

        assertTrue(graph.hasEdge(1, 2))
        assertFalse(graph.hasEdge(2, 1)) // Directed
        assertTrue(graph.directed)
    }

    // BFS Tests
    @Test
    fun `BFS traverse`() {
        val graph = Graph<Int>()
        graph.addEdge(1, 2)
        graph.addEdge(1, 3)
        graph.addEdge(2, 4)
        graph.addEdge(3, 4)

        val result = BFS.traverse(graph, 1)
        assertEquals(1, result[0])
        assertEquals(4, result.size)
        assertTrue(result.contains(2))
        assertTrue(result.contains(3))
        assertTrue(result.contains(4))
    }

    @Test
    fun `BFS shortest path`() {
        val graph = Graph<Int>()
        graph.addEdge(1, 2)
        graph.addEdge(1, 3)
        graph.addEdge(2, 4)
        graph.addEdge(3, 4)
        graph.addEdge(4, 5)

        val path = BFS.shortestPath(graph, 1, 5)
        assertNotNull(path)
        assertEquals(1, path[0])
        assertEquals(5, path.last())
        assertEquals(4, path.size) // 1 -> 2 or 3 -> 4 -> 5
    }

    @Test
    fun `BFS distances`() {
        val graph = Graph<Int>()
        graph.addEdge(1, 2)
        graph.addEdge(1, 3)
        graph.addEdge(2, 4)
        graph.addEdge(3, 4)
        graph.addEdge(4, 5)

        val distances = BFS.distances(graph, 1)
        assertEquals(0, distances[1])
        assertEquals(1, distances[2])
        assertEquals(1, distances[3])
        assertEquals(2, distances[4])
        assertEquals(3, distances[5])
    }

    // DFS Tests
    @Test
    fun `DFS traverse`() {
        val graph = Graph<Int>()
        graph.addEdge(1, 2)
        graph.addEdge(1, 3)
        graph.addEdge(2, 4)
        graph.addEdge(3, 4)

        val result = DFS.traverse(graph, 1)
        assertEquals(1, result[0])
        assertEquals(4, result.size)
    }

    @Test
    fun `DFS traverse recursive`() {
        val graph = Graph<Int>()
        graph.addEdge(1, 2)
        graph.addEdge(1, 3)
        graph.addEdge(2, 4)

        val result = DFS.traverseRecursive(graph, 1)
        assertEquals(1, result[0])
        assertEquals(4, result.size)
    }

    @Test
    fun `DFS find path`() {
        val graph = Graph<Int>()
        graph.addEdge(1, 2)
        graph.addEdge(2, 3)
        graph.addEdge(3, 4)

        val path = DFS.findPath(graph, 1, 4)
        assertNotNull(path)
        assertEquals(1, path[0])
        assertEquals(4, path.last())
    }

    @Test
    fun `DFS has cycle`() {
        val graphWithCycle = Graph<Int>(directed = true)
        graphWithCycle.addEdge(1, 2)
        graphWithCycle.addEdge(2, 3)
        graphWithCycle.addEdge(3, 1)

        assertTrue(DFS.hasCycle(graphWithCycle))

        val graphWithoutCycle = Graph<Int>(directed = true)
        graphWithoutCycle.addEdge(1, 2)
        graphWithoutCycle.addEdge(2, 3)

        assertFalse(DFS.hasCycle(graphWithoutCycle))
    }

    @Test
    fun `DFS topological sort`() {
        val graph = Graph<Int>(directed = true)
        graph.addEdge(5, 2)
        graph.addEdge(5, 0)
        graph.addEdge(4, 0)
        graph.addEdge(4, 1)
        graph.addEdge(2, 3)
        graph.addEdge(3, 1)

        val sorted = DFS.topologicalSort(graph)
        assertNotNull(sorted)

        // Verify ordering
        val positions = sorted.withIndex().associate { it.value to it.index }
        assertTrue(positions[5]!! < positions[2]!!)
        assertTrue(positions[2]!! < positions[3]!!)
        assertTrue(positions[3]!! < positions[1]!!)
    }
}
