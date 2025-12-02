import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import algorithms.*

class AlgorithmsTest extends AnyFunSuite with Matchers:

  // BinarySearch Tests
  test("BinarySearch - search"):
    val arr = Array(1, 3, 5, 7, 9, 11, 13)
    BinarySearch.search(arr, 7) shouldBe 3
    BinarySearch.search(arr, 1) shouldBe 0
    BinarySearch.search(arr, 13) shouldBe 6
    BinarySearch.search(arr, 6) shouldBe -1

  test("BinarySearch - lowerBound"):
    val arr = Array(1, 2, 2, 2, 3, 4)
    BinarySearch.lowerBound(arr, 2) shouldBe 1
    BinarySearch.lowerBound(arr, 0) shouldBe 0
    BinarySearch.lowerBound(arr, 5) shouldBe 6

  test("BinarySearch - upperBound"):
    val arr = Array(1, 2, 2, 2, 3, 4)
    BinarySearch.upperBound(arr, 2) shouldBe 4
    BinarySearch.upperBound(arr, 0) shouldBe 0
    BinarySearch.upperBound(arr, 4) shouldBe 6

  test("BinarySearch - contains"):
    val arr = Array(1, 3, 5, 7, 9)
    BinarySearch.contains(arr, 5) shouldBe true
    BinarySearch.contains(arr, 6) shouldBe false

  // InsertionSort Tests
  test("InsertionSort - sort"):
    val arr = Array(5, 2, 8, 1, 9, 3)
    InsertionSort.sort(arr)
    arr shouldBe Array(1, 2, 3, 5, 8, 9)

  test("InsertionSort - sorted (immutable)"):
    val arr = Array(5, 2, 8, 1, 9, 3)
    val result = InsertionSort.sorted(arr)
    result shouldBe Array(1, 2, 3, 5, 8, 9)
    arr shouldBe Array(5, 2, 8, 1, 9, 3)

  test("InsertionSort - already sorted"):
    val arr = Array(1, 2, 3, 4, 5)
    InsertionSort.sort(arr)
    arr shouldBe Array(1, 2, 3, 4, 5)

  test("InsertionSort - empty array"):
    val arr = Array.empty[Int]
    InsertionSort.sort(arr)
    arr shouldBe Array.empty[Int]

  // MergeSort Tests
  test("MergeSort - sort"):
    val arr = Array(5, 2, 8, 1, 9, 3)
    MergeSort.sort(arr)
    arr shouldBe Array(1, 2, 3, 5, 8, 9)

  test("MergeSort - sorted (immutable)"):
    val arr = Array(5, 2, 8, 1, 9, 3)
    val result = MergeSort.sorted(arr)
    result shouldBe Array(1, 2, 3, 5, 8, 9)
    arr shouldBe Array(5, 2, 8, 1, 9, 3)

  test("MergeSort - with duplicates"):
    val arr = Array(3, 1, 4, 1, 5, 9, 2, 6, 5)
    MergeSort.sort(arr)
    arr shouldBe Array(1, 1, 2, 3, 4, 5, 5, 6, 9)

  // QuickSort Tests
  test("QuickSort - sort"):
    val arr = Array(5, 2, 8, 1, 9, 3)
    QuickSort.sort(arr)
    arr shouldBe Array(1, 2, 3, 5, 8, 9)

  test("QuickSort - sorted (immutable)"):
    val arr = Array(5, 2, 8, 1, 9, 3)
    val result = QuickSort.sorted(arr)
    result shouldBe Array(1, 2, 3, 5, 8, 9)
    arr shouldBe Array(5, 2, 8, 1, 9, 3)

  test("QuickSort - reverse sorted"):
    val arr = Array(9, 8, 7, 6, 5, 4, 3, 2, 1)
    QuickSort.sort(arr)
    arr shouldBe Array(1, 2, 3, 4, 5, 6, 7, 8, 9)

  // Graph Tests
  test("Graph - add vertices and edges"):
    val graph = Graph[Int]()
    graph.addVertex(1).addVertex(2).addEdge(1, 2)
    graph.hasVertex(1) shouldBe true
    graph.hasVertex(3) shouldBe false
    graph.hasEdge(1, 2) shouldBe true
    graph.hasEdge(2, 1) shouldBe true

  test("Graph - directed"):
    val graph = Graph[Int](directed = true)
    graph.addEdge(1, 2)
    graph.hasEdge(1, 2) shouldBe true
    graph.hasEdge(2, 1) shouldBe false

  test("Graph - neighbors"):
    val graph = Graph[Int]()
    graph.addEdge(1, 2).addEdge(1, 3).addEdge(1, 4)
    graph.neighbors(1) shouldBe Set(2, 3, 4)

  test("Graph - remove vertex and edge"):
    val graph = Graph[Int]()
    graph.addEdge(1, 2).addEdge(2, 3)
    graph.removeEdge(1, 2)
    graph.hasEdge(1, 2) shouldBe false
    graph.removeVertex(2)
    graph.hasVertex(2) shouldBe false

  // BFS Tests
  test("BFS - traverse"):
    val graph = Graph[Int]()
    graph.addEdge(1, 2).addEdge(1, 3).addEdge(2, 4).addEdge(3, 4)
    val result = BFS.traverse(graph, 1)
    result.head shouldBe 1
    result should contain allOf (1, 2, 3, 4)

  test("BFS - shortestPath"):
    val graph = Graph[Int]()
    graph.addEdge(1, 2).addEdge(2, 3).addEdge(1, 3)
    BFS.shortestPath(graph, 1, 3) shouldBe Some(List(1, 3))
    BFS.shortestPath(graph, 1, 4) shouldBe None

  test("BFS - distances"):
    val graph = Graph[Int]()
    graph.addEdge(1, 2).addEdge(2, 3).addEdge(1, 4)
    val distances = BFS.distances(graph, 1)
    distances(1) shouldBe 0
    distances(2) shouldBe 1
    distances(3) shouldBe 2
    distances(4) shouldBe 1

  // DFS Tests
  test("DFS - traverse"):
    val graph = Graph[Int]()
    graph.addEdge(1, 2).addEdge(1, 3).addEdge(2, 4).addEdge(3, 4)
    val result = DFS.traverse(graph, 1)
    result.head shouldBe 1
    result should contain allOf (1, 2, 3, 4)

  test("DFS - traverseRecursive"):
    val graph = Graph[Int]()
    graph.addEdge(1, 2).addEdge(1, 3).addEdge(2, 4)
    val result = DFS.traverseRecursive(graph, 1)
    result.head shouldBe 1
    result should contain allOf (1, 2, 3, 4)

  test("DFS - findPath"):
    val graph = Graph[Int]()
    graph.addEdge(1, 2).addEdge(2, 3).addEdge(3, 4)
    val path = DFS.findPath(graph, 1, 4)
    path.isDefined shouldBe true
    path.get.head shouldBe 1
    path.get.last shouldBe 4

  test("DFS - hasCycle"):
    val directedWithCycle = Graph[Int](directed = true)
    directedWithCycle.addEdge(1, 2).addEdge(2, 3).addEdge(3, 1)
    DFS.hasCycle(directedWithCycle) shouldBe true

    val directedWithoutCycle = Graph[Int](directed = true)
    directedWithoutCycle.addEdge(1, 2).addEdge(2, 3)
    DFS.hasCycle(directedWithoutCycle) shouldBe false

  test("DFS - topologicalSort"):
    val dag = Graph[Int](directed = true)
    dag.addEdge(1, 2).addEdge(1, 3).addEdge(2, 4).addEdge(3, 4)
    val result = DFS.topologicalSort(dag)
    result.isDefined shouldBe true
    val sorted = result.get
    sorted.indexOf(1) should be < sorted.indexOf(2)
    sorted.indexOf(1) should be < sorted.indexOf(3)
    sorted.indexOf(2) should be < sorted.indexOf(4)
    sorted.indexOf(3) should be < sorted.indexOf(4)

  test("DFS - topologicalSort with cycle"):
    val graphWithCycle = Graph[Int](directed = true)
    graphWithCycle.addEdge(1, 2).addEdge(2, 3).addEdge(3, 1)
    DFS.topologicalSort(graphWithCycle) shouldBe None

  test("DFS - topologicalSort on undirected"):
    val undirected = Graph[Int](directed = false)
    DFS.topologicalSort(undirected) shouldBe None
