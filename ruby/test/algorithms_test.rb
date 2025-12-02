# frozen_string_literal: true

require 'minitest/autorun'
require_relative '../lib/algorithms/binary_search'
require_relative '../lib/algorithms/insertion_sort'
require_relative '../lib/algorithms/merge_sort'
require_relative '../lib/algorithms/quick_sort'
require_relative '../lib/algorithms/graph'
require_relative '../lib/algorithms/bfs'
require_relative '../lib/algorithms/dfs'

class AlgorithmsTest < Minitest::Test
  # Binary Search Tests
  def test_binary_search
    arr = [1, 3, 5, 7, 9, 11, 13]

    assert_equal 0, DSA::Algorithms::BinarySearch.search(arr, 1)
    assert_equal 3, DSA::Algorithms::BinarySearch.search(arr, 7)
    assert_equal 6, DSA::Algorithms::BinarySearch.search(arr, 13)
    assert_equal(-1, DSA::Algorithms::BinarySearch.search(arr, 4))
    assert_equal(-1, DSA::Algorithms::BinarySearch.search(arr, 0))
    assert_equal(-1, DSA::Algorithms::BinarySearch.search(arr, 14))
  end

  def test_binary_search_lower_bound
    arr = [1, 3, 5, 7, 9]

    assert_equal 0, DSA::Algorithms::BinarySearch.lower_bound(arr, 1)
    assert_equal 2, DSA::Algorithms::BinarySearch.lower_bound(arr, 5)
    assert_equal 2, DSA::Algorithms::BinarySearch.lower_bound(arr, 4) # Between 3 and 5
    assert_equal 0, DSA::Algorithms::BinarySearch.lower_bound(arr, 0) # Before all
    assert_equal 5, DSA::Algorithms::BinarySearch.lower_bound(arr, 10) # After all
  end

  def test_binary_search_upper_bound
    arr = [1, 3, 5, 7, 9]

    assert_equal 1, DSA::Algorithms::BinarySearch.upper_bound(arr, 1)
    assert_equal 3, DSA::Algorithms::BinarySearch.upper_bound(arr, 5)
    assert_equal 2, DSA::Algorithms::BinarySearch.upper_bound(arr, 4) # Between 3 and 5
    assert_equal 0, DSA::Algorithms::BinarySearch.upper_bound(arr, 0) # Before all
    assert_equal 5, DSA::Algorithms::BinarySearch.upper_bound(arr, 10) # After all
  end

  # Insertion Sort Tests
  def test_insertion_sort
    arr = [64, 34, 25, 12, 22, 11, 90]
    DSA::Algorithms::InsertionSort.sort!(arr)
    assert_equal [11, 12, 22, 25, 34, 64, 90], arr
  end

  def test_insertion_sort_non_destructive
    arr = [5, 2, 8, 1, 9]
    sorted = DSA::Algorithms::InsertionSort.sort(arr)
    assert_equal [1, 2, 5, 8, 9], sorted
    assert_equal [5, 2, 8, 1, 9], arr # Original unchanged
  end

  def test_insertion_sort_descending
    arr = [3, 1, 4, 1, 5]
    DSA::Algorithms::InsertionSort.sort!(arr) { |a, b| b <=> a }
    assert_equal [5, 4, 3, 1, 1], arr
  end

  # Merge Sort Tests
  def test_merge_sort
    arr = [64, 34, 25, 12, 22, 11, 90]
    DSA::Algorithms::MergeSort.sort!(arr)
    assert_equal [11, 12, 22, 25, 34, 64, 90], arr
  end

  def test_merge_sort_non_destructive
    arr = [5, 2, 8, 1, 9]
    sorted = DSA::Algorithms::MergeSort.sort(arr)
    assert_equal [1, 2, 5, 8, 9], sorted
  end

  def test_merge_sort_empty
    arr = []
    DSA::Algorithms::MergeSort.sort!(arr)
    assert_equal [], arr
  end

  def test_merge_sort_single_element
    arr = [42]
    DSA::Algorithms::MergeSort.sort!(arr)
    assert_equal [42], arr
  end

  # Quick Sort Tests
  def test_quick_sort
    arr = [64, 34, 25, 12, 22, 11, 90]
    DSA::Algorithms::QuickSort.sort!(arr)
    assert_equal [11, 12, 22, 25, 34, 64, 90], arr
  end

  def test_quick_sort_non_destructive
    arr = [5, 2, 8, 1, 9]
    sorted = DSA::Algorithms::QuickSort.sort(arr)
    assert_equal [1, 2, 5, 8, 9], sorted
  end

  def test_quick_sort_with_duplicates
    arr = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
    DSA::Algorithms::QuickSort.sort!(arr)
    assert_equal [1, 1, 2, 3, 3, 4, 5, 5, 6, 9], arr
  end

  # Graph Tests
  def test_graph_operations
    graph = DSA::Algorithms::Graph.new

    graph.add_edge(1, 2).add_edge(1, 3).add_edge(2, 4)

    assert graph.has_edge?(1, 2)
    assert graph.has_edge?(2, 1) # Undirected
    refute graph.has_edge?(1, 4)

    assert_equal [2, 3], graph.neighbors(1)

    graph.remove_edge(1, 2)
    refute graph.has_edge?(1, 2)
  end

  def test_directed_graph
    graph = DSA::Algorithms::Graph.new(directed: true)

    graph.add_edge(1, 2).add_edge(2, 3)

    assert graph.has_edge?(1, 2)
    refute graph.has_edge?(2, 1) # Directed
    assert graph.directed
  end

  # BFS Tests
  def test_bfs_traverse
    graph = DSA::Algorithms::Graph.new
    graph.add_edge(1, 2).add_edge(1, 3).add_edge(2, 4).add_edge(3, 4)

    result = DSA::Algorithms::BFS.traverse(graph, 1)
    assert_equal 1, result.first
    assert_equal 4, result.size
    assert_includes result, 2
    assert_includes result, 3
    assert_includes result, 4
  end

  def test_bfs_shortest_path
    graph = DSA::Algorithms::Graph.new
    graph.add_edge(1, 2).add_edge(1, 3).add_edge(2, 4).add_edge(3, 4).add_edge(4, 5)

    path = DSA::Algorithms::BFS.shortest_path(graph, 1, 5)
    refute_nil path
    assert_equal 1, path.first
    assert_equal 5, path.last
    assert_equal 4, path.size # 1 -> 2 or 3 -> 4 -> 5
  end

  def test_bfs_distances
    graph = DSA::Algorithms::Graph.new
    graph.add_edge(1, 2).add_edge(1, 3).add_edge(2, 4).add_edge(3, 4).add_edge(4, 5)

    distances = DSA::Algorithms::BFS.distances(graph, 1)
    assert_equal 0, distances[1]
    assert_equal 1, distances[2]
    assert_equal 1, distances[3]
    assert_equal 2, distances[4]
    assert_equal 3, distances[5]
  end

  # DFS Tests
  def test_dfs_traverse
    graph = DSA::Algorithms::Graph.new
    graph.add_edge(1, 2).add_edge(1, 3).add_edge(2, 4).add_edge(3, 4)

    result = DSA::Algorithms::DFS.traverse(graph, 1)
    assert_equal 1, result.first
    assert_equal 4, result.size
  end

  def test_dfs_traverse_recursive
    graph = DSA::Algorithms::Graph.new
    graph.add_edge(1, 2).add_edge(1, 3).add_edge(2, 4)

    result = DSA::Algorithms::DFS.traverse_recursive(graph, 1)
    assert_equal 1, result.first
    assert_equal 4, result.size
  end

  def test_dfs_find_path
    graph = DSA::Algorithms::Graph.new
    graph.add_edge(1, 2).add_edge(2, 3).add_edge(3, 4)

    path = DSA::Algorithms::DFS.find_path(graph, 1, 4)
    refute_nil path
    assert_equal 1, path.first
    assert_equal 4, path.last
  end

  def test_dfs_has_cycle
    graph_with_cycle = DSA::Algorithms::Graph.new(directed: true)
    graph_with_cycle.add_edge(1, 2).add_edge(2, 3).add_edge(3, 1)

    assert DSA::Algorithms::DFS.has_cycle?(graph_with_cycle)

    graph_without_cycle = DSA::Algorithms::Graph.new(directed: true)
    graph_without_cycle.add_edge(1, 2).add_edge(2, 3)

    refute DSA::Algorithms::DFS.has_cycle?(graph_without_cycle)
  end

  def test_dfs_topological_sort
    graph = DSA::Algorithms::Graph.new(directed: true)
    graph.add_edge(5, 2).add_edge(5, 0).add_edge(4, 0)
    graph.add_edge(4, 1).add_edge(2, 3).add_edge(3, 1)

    sorted = DSA::Algorithms::DFS.topological_sort(graph)
    refute_nil sorted

    # Verify ordering
    positions = sorted.each_with_index.to_h
    assert positions[5] < positions[2]
    assert positions[2] < positions[3]
    assert positions[3] < positions[1]
  end
end
