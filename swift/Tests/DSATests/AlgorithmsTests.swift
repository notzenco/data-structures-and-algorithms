import XCTest
@testable import DSA

final class AlgorithmsTests: XCTestCase {
    // MARK: - Binary Search Tests
    func testBinarySearch() {
        let arr = [1, 3, 5, 7, 9, 11, 13]

        XCTAssertEqual(BinarySearch.search(arr, 1), 0)
        XCTAssertEqual(BinarySearch.search(arr, 7), 3)
        XCTAssertEqual(BinarySearch.search(arr, 13), 6)
        XCTAssertEqual(BinarySearch.search(arr, 4), -1)
        XCTAssertEqual(BinarySearch.search(arr, 0), -1)
        XCTAssertEqual(BinarySearch.search(arr, 14), -1)
    }

    func testBinarySearchLowerBound() {
        let arr = [1, 3, 5, 7, 9]

        XCTAssertEqual(BinarySearch.lowerBound(arr, 1), 0)
        XCTAssertEqual(BinarySearch.lowerBound(arr, 5), 2)
        XCTAssertEqual(BinarySearch.lowerBound(arr, 4), 2) // Between 3 and 5
        XCTAssertEqual(BinarySearch.lowerBound(arr, 0), 0) // Before all
        XCTAssertEqual(BinarySearch.lowerBound(arr, 10), 5) // After all
    }

    func testBinarySearchUpperBound() {
        let arr = [1, 3, 5, 7, 9]

        XCTAssertEqual(BinarySearch.upperBound(arr, 1), 1)
        XCTAssertEqual(BinarySearch.upperBound(arr, 5), 3)
        XCTAssertEqual(BinarySearch.upperBound(arr, 4), 2) // Between 3 and 5
        XCTAssertEqual(BinarySearch.upperBound(arr, 0), 0) // Before all
        XCTAssertEqual(BinarySearch.upperBound(arr, 10), 5) // After all
    }

    // MARK: - Insertion Sort Tests
    func testInsertionSort() {
        var arr = [64, 34, 25, 12, 22, 11, 90]
        InsertionSort.sort(&arr)
        XCTAssertEqual(arr, [11, 12, 22, 25, 34, 64, 90])
    }

    func testInsertionSortSorted() {
        let arr = [5, 2, 8, 1, 9]
        let sorted = InsertionSort.sorted(arr)
        XCTAssertEqual(sorted, [1, 2, 5, 8, 9])
        XCTAssertEqual(arr, [5, 2, 8, 1, 9]) // Original unchanged
    }

    func testInsertionSortDescending() {
        var arr = [3, 1, 4, 1, 5]
        InsertionSort.sort(&arr) { $0 > $1 }
        XCTAssertEqual(arr, [5, 4, 3, 1, 1])
    }

    // MARK: - Merge Sort Tests
    func testMergeSort() {
        var arr = [64, 34, 25, 12, 22, 11, 90]
        MergeSort.sort(&arr)
        XCTAssertEqual(arr, [11, 12, 22, 25, 34, 64, 90])
    }

    func testMergeSortSorted() {
        let arr = [5, 2, 8, 1, 9]
        let sorted = MergeSort.sorted(arr)
        XCTAssertEqual(sorted, [1, 2, 5, 8, 9])
    }

    func testMergeSortEmpty() {
        var arr: [Int] = []
        MergeSort.sort(&arr)
        XCTAssertEqual(arr, [])
    }

    func testMergeSortSingleElement() {
        var arr = [42]
        MergeSort.sort(&arr)
        XCTAssertEqual(arr, [42])
    }

    // MARK: - Quick Sort Tests
    func testQuickSort() {
        var arr = [64, 34, 25, 12, 22, 11, 90]
        QuickSort.sort(&arr)
        XCTAssertEqual(arr, [11, 12, 22, 25, 34, 64, 90])
    }

    func testQuickSortSorted() {
        let arr = [5, 2, 8, 1, 9]
        let sorted = QuickSort.sorted(arr)
        XCTAssertEqual(sorted, [1, 2, 5, 8, 9])
    }

    func testQuickSortWithDuplicates() {
        var arr = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
        QuickSort.sort(&arr)
        XCTAssertEqual(arr, [1, 1, 2, 3, 3, 4, 5, 5, 6, 9])
    }

    // MARK: - Graph Tests
    func testGraphOperations() {
        let graph = Graph<Int>()

        graph.addEdge(from: 1, to: 2)
        graph.addEdge(from: 1, to: 3)
        graph.addEdge(from: 2, to: 4)

        XCTAssertTrue(graph.hasEdge(from: 1, to: 2))
        XCTAssertTrue(graph.hasEdge(from: 2, to: 1)) // Undirected
        XCTAssertFalse(graph.hasEdge(from: 1, to: 4))

        XCTAssertEqual(graph.getNeighbors(1), [2, 3])

        graph.removeEdge(from: 1, to: 2)
        XCTAssertFalse(graph.hasEdge(from: 1, to: 2))
    }

    func testDirectedGraph() {
        let graph = Graph<Int>(directed: true)

        graph.addEdge(from: 1, to: 2)
        graph.addEdge(from: 2, to: 3)

        XCTAssertTrue(graph.hasEdge(from: 1, to: 2))
        XCTAssertFalse(graph.hasEdge(from: 2, to: 1)) // Directed
        XCTAssertTrue(graph.directed)
    }

    // MARK: - BFS Tests
    func testBFSTraverse() {
        let graph = Graph<Int>()
        graph.addEdge(from: 1, to: 2)
        graph.addEdge(from: 1, to: 3)
        graph.addEdge(from: 2, to: 4)
        graph.addEdge(from: 3, to: 4)

        let result = BFS.traverse(graph, from: 1)
        XCTAssertEqual(result.first, 1)
        XCTAssertEqual(result.count, 4)
        XCTAssertTrue(result.contains(2))
        XCTAssertTrue(result.contains(3))
        XCTAssertTrue(result.contains(4))
    }

    func testBFSShortestPath() {
        let graph = Graph<Int>()
        graph.addEdge(from: 1, to: 2)
        graph.addEdge(from: 1, to: 3)
        graph.addEdge(from: 2, to: 4)
        graph.addEdge(from: 3, to: 4)
        graph.addEdge(from: 4, to: 5)

        let path = BFS.shortestPath(graph, from: 1, to: 5)
        XCTAssertNotNil(path)
        XCTAssertEqual(path?.first, 1)
        XCTAssertEqual(path?.last, 5)
        XCTAssertEqual(path?.count, 4) // 1 -> 2 or 3 -> 4 -> 5
    }

    func testBFSDistances() {
        let graph = Graph<Int>()
        graph.addEdge(from: 1, to: 2)
        graph.addEdge(from: 1, to: 3)
        graph.addEdge(from: 2, to: 4)
        graph.addEdge(from: 3, to: 4)
        graph.addEdge(from: 4, to: 5)

        let distances = BFS.distances(graph, from: 1)
        XCTAssertEqual(distances[1], 0)
        XCTAssertEqual(distances[2], 1)
        XCTAssertEqual(distances[3], 1)
        XCTAssertEqual(distances[4], 2)
        XCTAssertEqual(distances[5], 3)
    }

    // MARK: - DFS Tests
    func testDFSTraverse() {
        let graph = Graph<Int>()
        graph.addEdge(from: 1, to: 2)
        graph.addEdge(from: 1, to: 3)
        graph.addEdge(from: 2, to: 4)
        graph.addEdge(from: 3, to: 4)

        let result = DFS.traverse(graph, from: 1)
        XCTAssertEqual(result.first, 1)
        XCTAssertEqual(result.count, 4)
    }

    func testDFSTraverseRecursive() {
        let graph = Graph<Int>()
        graph.addEdge(from: 1, to: 2)
        graph.addEdge(from: 1, to: 3)
        graph.addEdge(from: 2, to: 4)

        let result = DFS.traverseRecursive(graph, from: 1)
        XCTAssertEqual(result.first, 1)
        XCTAssertEqual(result.count, 4)
    }

    func testDFSFindPath() {
        let graph = Graph<Int>()
        graph.addEdge(from: 1, to: 2)
        graph.addEdge(from: 2, to: 3)
        graph.addEdge(from: 3, to: 4)

        let path = DFS.findPath(graph, from: 1, to: 4)
        XCTAssertNotNil(path)
        XCTAssertEqual(path?.first, 1)
        XCTAssertEqual(path?.last, 4)
    }

    func testDFSHasCycle() {
        let graphWithCycle = Graph<Int>(directed: true)
        graphWithCycle.addEdge(from: 1, to: 2)
        graphWithCycle.addEdge(from: 2, to: 3)
        graphWithCycle.addEdge(from: 3, to: 1)

        XCTAssertTrue(DFS.hasCycle(graphWithCycle))

        let graphWithoutCycle = Graph<Int>(directed: true)
        graphWithoutCycle.addEdge(from: 1, to: 2)
        graphWithoutCycle.addEdge(from: 2, to: 3)

        XCTAssertFalse(DFS.hasCycle(graphWithoutCycle))
    }

    func testDFSTopologicalSort() {
        let graph = Graph<Int>(directed: true)
        graph.addEdge(from: 5, to: 2)
        graph.addEdge(from: 5, to: 0)
        graph.addEdge(from: 4, to: 0)
        graph.addEdge(from: 4, to: 1)
        graph.addEdge(from: 2, to: 3)
        graph.addEdge(from: 3, to: 1)

        let sorted = DFS.topologicalSort(graph)
        XCTAssertNotNil(sorted)

        // Verify ordering
        if let sorted = sorted {
            let positions = Dictionary(uniqueKeysWithValues: sorted.enumerated().map { ($0.element, $0.offset) })
            XCTAssertLessThan(positions[5]!, positions[2]!)
            XCTAssertLessThan(positions[2]!, positions[3]!)
            XCTAssertLessThan(positions[3]!, positions[1]!)
        }
    }
}
