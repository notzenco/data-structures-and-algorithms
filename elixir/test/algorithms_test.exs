defmodule AlgorithmsTest do
  use ExUnit.Case, async: true

  alias Algorithms.{
    BinarySearch,
    InsertionSort,
    MergeSort,
    QuickSort,
    Graph,
    BFS,
    DFS
  }

  # BinarySearch Tests
  describe "BinarySearch" do
    test "search finds existing elements" do
      arr = [1, 3, 5, 7, 9, 11, 13]
      assert BinarySearch.search(arr, 7) == 3
      assert BinarySearch.search(arr, 1) == 0
      assert BinarySearch.search(arr, 13) == 6
    end

    test "search returns nil for non-existing elements" do
      arr = [1, 3, 5, 7, 9]
      assert BinarySearch.search(arr, 6) == nil
    end

    test "lower_bound finds first >= target" do
      arr = [1, 2, 2, 2, 3, 4]
      assert BinarySearch.lower_bound(arr, 2) == 1
    end

    test "upper_bound finds first > target" do
      arr = [1, 2, 2, 2, 3, 4]
      assert BinarySearch.upper_bound(arr, 2) == 4
    end
  end

  # InsertionSort Tests
  describe "InsertionSort" do
    test "sorts a list" do
      assert InsertionSort.sort([5, 2, 8, 1, 9, 3]) == [1, 2, 3, 5, 8, 9]
    end

    test "handles empty list" do
      assert InsertionSort.sort([]) == []
    end

    test "handles already sorted list" do
      assert InsertionSort.sort([1, 2, 3, 4, 5]) == [1, 2, 3, 4, 5]
    end

    test "handles list with duplicates" do
      assert InsertionSort.sort([3, 1, 4, 1, 5, 9, 2, 6, 5]) == [1, 1, 2, 3, 4, 5, 5, 6, 9]
    end
  end

  # MergeSort Tests
  describe "MergeSort" do
    test "sorts a list" do
      assert MergeSort.sort([5, 2, 8, 1, 9, 3]) == [1, 2, 3, 5, 8, 9]
    end

    test "handles empty list" do
      assert MergeSort.sort([]) == []
    end

    test "handles single element" do
      assert MergeSort.sort([42]) == [42]
    end
  end

  # QuickSort Tests
  describe "QuickSort" do
    test "sorts a list" do
      assert QuickSort.sort([5, 2, 8, 1, 9, 3]) == [1, 2, 3, 5, 8, 9]
    end

    test "handles reverse sorted list" do
      assert QuickSort.sort([9, 8, 7, 6, 5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
    end
  end

  # Graph Tests
  describe "Graph" do
    test "adds vertices and edges" do
      graph =
        Graph.undirected()
        |> Graph.add_vertex(3)
        |> Graph.add_edge(1, 2)

      assert Graph.has_vertex?(graph, 1)
      assert Graph.has_vertex?(graph, 3)
      assert Graph.has_edge?(graph, 1, 2)
      assert Graph.has_edge?(graph, 2, 1)
    end

    test "directed graph has one-way edges" do
      graph =
        Graph.directed()
        |> Graph.add_edge(1, 2)

      assert Graph.has_edge?(graph, 1, 2)
      refute Graph.has_edge?(graph, 2, 1)
    end
  end

  # BFS Tests
  describe "BFS" do
    test "traverses all reachable vertices" do
      graph =
        Graph.undirected()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(1, 3)
        |> Graph.add_edge(2, 4)

      result = BFS.traverse(graph, 1)
      assert length(result) == 4
      assert hd(result) == 1
    end

    test "finds shortest path" do
      graph =
        Graph.undirected()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(2, 3)
        |> Graph.add_edge(1, 3)

      assert {:ok, [1, 3]} = BFS.shortest_path(graph, 1, 3)
    end

    test "returns error for unreachable vertices" do
      graph =
        Graph.undirected()
        |> Graph.add_edge(1, 2)
        |> Graph.add_vertex(4)

      assert {:error, :not_found} = BFS.shortest_path(graph, 1, 4)
    end

    test "calculates distances correctly" do
      graph =
        Graph.undirected()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(2, 3)
        |> Graph.add_edge(1, 4)

      distances = BFS.distances(graph, 1)
      assert distances[1] == 0
      assert distances[2] == 1
      assert distances[3] == 2
      assert distances[4] == 1
    end
  end

  # DFS Tests
  describe "DFS" do
    test "traverses all reachable vertices" do
      graph =
        Graph.undirected()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(1, 3)
        |> Graph.add_edge(2, 4)

      result = DFS.traverse(graph, 1)
      assert length(result) == 4
      assert hd(result) == 1
    end

    test "finds a path between vertices" do
      graph =
        Graph.undirected()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(2, 3)
        |> Graph.add_edge(3, 4)

      assert {:ok, path} = DFS.find_path(graph, 1, 4)
      assert hd(path) == 1
      assert List.last(path) == 4
    end

    test "detects cycles in directed graph" do
      cyclic =
        Graph.directed()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(2, 3)
        |> Graph.add_edge(3, 1)

      assert DFS.has_cycle?(cyclic)

      acyclic =
        Graph.directed()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(2, 3)

      refute DFS.has_cycle?(acyclic)
    end

    test "topological sort returns valid ordering" do
      dag =
        Graph.directed()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(1, 3)
        |> Graph.add_edge(2, 4)
        |> Graph.add_edge(3, 4)

      assert {:ok, sorted} = DFS.topological_sort(dag)
      idx = fn x -> Enum.find_index(sorted, &(&1 == x)) end
      assert idx.(1) < idx.(2)
      assert idx.(1) < idx.(3)
      assert idx.(2) < idx.(4)
      assert idx.(3) < idx.(4)
    end

    test "topological sort returns error for cyclic graph" do
      cyclic =
        Graph.directed()
        |> Graph.add_edge(1, 2)
        |> Graph.add_edge(2, 3)
        |> Graph.add_edge(3, 1)

      assert {:error, :has_cycle} = DFS.topological_sort(cyclic)
    end

    test "topological sort returns error for undirected graph" do
      undirected = Graph.undirected()
      assert {:error, :not_directed} = DFS.topological_sort(undirected)
    end
  end
end
