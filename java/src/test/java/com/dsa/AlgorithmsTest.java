package com.dsa;

import com.dsa.algorithms.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

class AlgorithmsTest {

    // Binary Search Tests
    @Test
    void binarySearchFindsElement() {
        List<Integer> arr = Arrays.asList(1, 3, 5, 7, 9, 11, 13);

        assertEquals(Optional.of(0), BinarySearch.search(arr, 1));
        assertEquals(Optional.of(3), BinarySearch.search(arr, 7));
        assertEquals(Optional.of(6), BinarySearch.search(arr, 13));
    }

    @Test
    void binarySearchElementNotFound() {
        List<Integer> arr = Arrays.asList(1, 3, 5, 7, 9);

        assertEquals(Optional.empty(), BinarySearch.search(arr, 0));
        assertEquals(Optional.empty(), BinarySearch.search(arr, 4));
        assertEquals(Optional.empty(), BinarySearch.search(arr, 10));
    }

    @Test
    void binarySearchEmptyList() {
        List<Integer> arr = Collections.emptyList();
        assertEquals(Optional.empty(), BinarySearch.search(arr, 5));
    }

    @Test
    void binarySearchWithComparator() {
        List<String> arr = Arrays.asList("apple", "banana", "cherry", "date");
        Comparator<String> cmp = String::compareTo;

        assertEquals(Optional.of(1), BinarySearch.search(arr, "banana", cmp));
        assertEquals(Optional.empty(), BinarySearch.search(arr, "fig", cmp));
    }

    // Insertion Sort Tests
    @Test
    void insertionSortBasic() {
        List<Integer> arr = new ArrayList<>(Arrays.asList(5, 2, 8, 1, 9, 3));
        InsertionSort.sort(arr);
        assertEquals(Arrays.asList(1, 2, 3, 5, 8, 9), arr);
    }

    @Test
    void insertionSortAlreadySorted() {
        List<Integer> arr = new ArrayList<>(Arrays.asList(1, 2, 3, 4, 5));
        InsertionSort.sort(arr);
        assertEquals(Arrays.asList(1, 2, 3, 4, 5), arr);
    }

    @Test
    void insertionSortReversed() {
        List<Integer> arr = new ArrayList<>(Arrays.asList(5, 4, 3, 2, 1));
        InsertionSort.sort(arr);
        assertEquals(Arrays.asList(1, 2, 3, 4, 5), arr);
    }

    @Test
    void insertionSortEmpty() {
        List<Integer> arr = new ArrayList<>();
        InsertionSort.sort(arr);
        assertTrue(arr.isEmpty());
    }

    @Test
    void insertionSortSingleElement() {
        List<Integer> arr = new ArrayList<>(Collections.singletonList(42));
        InsertionSort.sort(arr);
        assertEquals(Collections.singletonList(42), arr);
    }

    // Merge Sort Tests
    @Test
    void mergeSortBasic() {
        List<Integer> arr = new ArrayList<>(Arrays.asList(5, 2, 8, 1, 9, 3));
        MergeSort.sort(arr);
        assertEquals(Arrays.asList(1, 2, 3, 5, 8, 9), arr);
    }

    @Test
    void mergeSortWithDuplicates() {
        List<Integer> arr = new ArrayList<>(Arrays.asList(3, 1, 4, 1, 5, 9, 2, 6, 5, 3));
        MergeSort.sort(arr);
        assertEquals(Arrays.asList(1, 1, 2, 3, 3, 4, 5, 5, 6, 9), arr);
    }

    @Test
    void mergeSortStrings() {
        List<String> arr = new ArrayList<>(Arrays.asList("banana", "apple", "cherry"));
        MergeSort.sort(arr, String::compareTo);
        assertEquals(Arrays.asList("apple", "banana", "cherry"), arr);
    }

    @Test
    void mergeSortDescending() {
        List<Integer> arr = new ArrayList<>(Arrays.asList(1, 5, 3, 2, 4));
        MergeSort.sortDescending(arr, Integer::compareTo);
        assertEquals(Arrays.asList(5, 4, 3, 2, 1), arr);
    }

    // Quick Sort Tests
    @Test
    void quickSortBasic() {
        List<Integer> arr = new ArrayList<>(Arrays.asList(5, 2, 8, 1, 9, 3));
        QuickSort.sort(arr);
        assertEquals(Arrays.asList(1, 2, 3, 5, 8, 9), arr);
    }

    @Test
    void quickSortWithDuplicates() {
        List<Integer> arr = new ArrayList<>(Arrays.asList(3, 1, 4, 1, 5, 9, 2, 6, 5, 3));
        QuickSort.sort(arr);
        assertEquals(Arrays.asList(1, 1, 2, 3, 3, 4, 5, 5, 6, 9), arr);
    }

    @Test
    void quickSortLargeArray() {
        List<Integer> arr = new ArrayList<>();
        Random rand = new Random(42);
        for (int i = 0; i < 1000; i++) {
            arr.add(rand.nextInt(1000));
        }

        List<Integer> expected = new ArrayList<>(arr);
        Collections.sort(expected);

        QuickSort.sort(arr);
        assertEquals(expected, arr);
    }

    @Test
    void quickSortStrings() {
        List<String> arr = new ArrayList<>(Arrays.asList("banana", "apple", "cherry"));
        QuickSort.sort(arr, String::compareTo);
        assertEquals(Arrays.asList("apple", "banana", "cherry"), arr);
    }

    // BFS Tests
    @Test
    void bfsTraversal() {
        Graph<String> graph = new Graph<>();
        graph.addEdge("A", "B");
        graph.addEdge("A", "C");
        graph.addEdge("B", "D");
        graph.addEdge("C", "D");

        List<String> result = BFS.traverse(graph, "A");
        assertEquals("A", result.get(0));
        assertTrue(result.contains("B"));
        assertTrue(result.contains("C"));
        assertTrue(result.contains("D"));
        assertEquals(4, result.size());
    }

    @Test
    void bfsWithCallback() {
        Graph<Integer> graph = new Graph<>();
        graph.addEdge(1, 2);
        graph.addEdge(1, 3);
        graph.addEdge(2, 4);

        List<Integer> visited = new ArrayList<>();
        BFS.traverse(graph, 1, visited::add);

        assertEquals(Integer.valueOf(1), visited.get(0));
        assertEquals(4, visited.size());
    }

    @Test
    void bfsFindPath() {
        Graph<String> graph = new Graph<>();
        graph.addEdge("A", "B");
        graph.addEdge("B", "C");
        graph.addEdge("C", "D");
        graph.addEdge("A", "D");

        Optional<List<String>> path = BFS.findPath(graph, "A", "D");
        assertTrue(path.isPresent());
        assertEquals("A", path.get().get(0));
        assertEquals("D", path.get().get(path.get().size() - 1));
        assertEquals(2, path.get().size()); // Shortest path A -> D
    }

    @Test
    void bfsNoPath() {
        Graph<String> graph = new Graph<>();
        graph.addVertex("A");
        graph.addVertex("B");

        Optional<List<String>> path = BFS.findPath(graph, "A", "B");
        assertTrue(path.isEmpty());
    }

    @Test
    void bfsDistances() {
        Graph<String> graph = new Graph<>();
        graph.addEdge("A", "B");
        graph.addEdge("A", "C");
        graph.addEdge("B", "D");
        graph.addEdge("C", "D");
        graph.addEdge("D", "E");

        Map<String, Integer> distances = BFS.distances(graph, "A");

        assertEquals(0, distances.get("A"));
        assertEquals(1, distances.get("B"));
        assertEquals(1, distances.get("C"));
        assertEquals(2, distances.get("D"));
        assertEquals(3, distances.get("E"));
    }

    // DFS Tests
    @Test
    void dfsTraversal() {
        Graph<String> graph = new Graph<>();
        graph.addEdge("A", "B");
        graph.addEdge("A", "C");
        graph.addEdge("B", "D");
        graph.addEdge("C", "D");

        List<String> result = DFS.traverse(graph, "A");
        assertEquals("A", result.get(0));
        assertTrue(result.contains("B"));
        assertTrue(result.contains("C"));
        assertTrue(result.contains("D"));
        assertEquals(4, result.size());
    }

    @Test
    void dfsRecursiveTraversal() {
        Graph<String> graph = new Graph<>();
        graph.addEdge("A", "B");
        graph.addEdge("A", "C");
        graph.addEdge("B", "D");

        List<String> iterative = DFS.traverse(graph, "A");
        List<String> recursive = DFS.traverseRecursive(graph, "A");

        assertEquals(iterative.size(), recursive.size());
        assertEquals(iterative.get(0), recursive.get(0)); // Both start at A
    }

    @Test
    void dfsFindPath() {
        Graph<String> graph = new Graph<>();
        graph.addEdge("A", "B");
        graph.addEdge("B", "C");
        graph.addEdge("C", "D");

        Optional<List<String>> path = DFS.findPath(graph, "A", "D");
        assertTrue(path.isPresent());
        assertEquals("A", path.get().get(0));
        assertEquals("D", path.get().get(path.get().size() - 1));
    }

    @Test
    void dfsCycleDetection() {
        Graph<String> directedGraph = new Graph<>(true);
        directedGraph.addEdge("A", "B");
        directedGraph.addEdge("B", "C");
        directedGraph.addEdge("C", "A");

        assertTrue(DFS.hasCycle(directedGraph, "A"));
    }

    @Test
    void dfsNoCycle() {
        Graph<String> directedGraph = new Graph<>(true);
        directedGraph.addEdge("A", "B");
        directedGraph.addEdge("B", "C");
        directedGraph.addEdge("A", "C");

        assertFalse(DFS.hasCycle(directedGraph, "A"));
    }

    @Test
    void dfsInvalidStart() {
        Graph<String> graph = new Graph<>();
        graph.addEdge("A", "B");

        List<String> result = DFS.traverse(graph, "X");
        assertTrue(result.isEmpty());
    }

    // Graph Tests
    @Test
    void graphBasicOperations() {
        Graph<Integer> graph = new Graph<>();
        graph.addVertex(1);
        graph.addVertex(2);

        assertTrue(graph.hasVertex(1));
        assertTrue(graph.hasVertex(2));
        assertFalse(graph.hasVertex(3));
        assertEquals(2, graph.size());
    }

    @Test
    void graphUndirected() {
        Graph<String> graph = new Graph<>(false);
        graph.addEdge("A", "B");

        assertTrue(graph.neighbors("A").contains("B"));
        assertTrue(graph.neighbors("B").contains("A"));
    }

    @Test
    void graphDirected() {
        Graph<String> graph = new Graph<>(true);
        graph.addEdge("A", "B");

        assertTrue(graph.neighbors("A").contains("B"));
        assertFalse(graph.neighbors("B").contains("A"));
    }
}
