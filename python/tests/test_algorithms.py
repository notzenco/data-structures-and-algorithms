"""Tests for algorithms."""

import pytest
from dsa import (
    binary_search, binary_search_recursive, lower_bound, upper_bound,
    insertion_sort, merge_sort, quick_sort,
    Graph, bfs, bfs_path, dfs, dfs_recursive, dfs_path
)
from dsa.algorithms.bfs import bfs_distances
from dsa.algorithms.dfs import has_cycle


class TestBinarySearch:
    def test_found(self):
        arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        assert binary_search(arr, 1) == 0
        assert binary_search(arr, 5) == 4
        assert binary_search(arr, 10) == 9

    def test_not_found(self):
        arr = [1, 3, 5, 7, 9]
        assert binary_search(arr, 0) is None
        assert binary_search(arr, 2) is None

    def test_empty(self):
        assert binary_search([], 5) is None

    def test_recursive(self):
        arr = [1, 2, 3, 4, 5]
        assert binary_search_recursive(arr, 3) == 2
        assert binary_search_recursive(arr, 6) is None

    def test_lower_bound(self):
        arr = [1, 2, 4, 4, 4, 6, 7]
        assert lower_bound(arr, 4) == 2
        assert lower_bound(arr, 3) == 2
        assert lower_bound(arr, 0) == 0

    def test_upper_bound(self):
        arr = [1, 2, 4, 4, 4, 6, 7]
        assert upper_bound(arr, 4) == 5
        assert upper_bound(arr, 3) == 2


class TestInsertionSort:
    def test_basic(self):
        arr = [5, 2, 8, 1, 9]
        insertion_sort(arr)
        assert arr == [1, 2, 5, 8, 9]

    def test_reverse(self):
        arr = [1, 5, 3, 2, 4]
        insertion_sort(arr, reverse=True)
        assert arr == [5, 4, 3, 2, 1]

    def test_with_key(self):
        arr = [(1, 'a'), (2, 'b'), (1, 'c')]
        insertion_sort(arr, key=lambda x: x[0])
        assert arr[0][1] == 'a'  # Stable sort

    def test_empty(self):
        arr = []
        insertion_sort(arr)
        assert arr == []


class TestMergeSort:
    def test_basic(self):
        arr = [5, 2, 8, 1, 9]
        merge_sort(arr)
        assert arr == [1, 2, 5, 8, 9]

    def test_reverse(self):
        arr = [1, 5, 3, 2, 4]
        merge_sort(arr, reverse=True)
        assert arr == [5, 4, 3, 2, 1]

    def test_stability(self):
        arr = [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd')]
        merge_sort(arr, key=lambda x: x[0])
        assert arr[0][1] == 'a'
        assert arr[1][1] == 'c'

    def test_large(self):
        arr = list(range(1000, 0, -1))
        merge_sort(arr)
        assert arr == list(range(1, 1001))


class TestQuickSort:
    def test_basic(self):
        arr = [5, 2, 8, 1, 9]
        quick_sort(arr)
        assert arr == [1, 2, 5, 8, 9]

    def test_reverse(self):
        arr = [1, 5, 3, 2, 4]
        quick_sort(arr, reverse=True)
        assert arr == [5, 4, 3, 2, 1]

    def test_duplicates(self):
        arr = [5, 2, 5, 3, 5, 1, 5]
        quick_sort(arr)
        assert all(arr[i] <= arr[i+1] for i in range(len(arr)-1))

    def test_large(self):
        arr = list(range(1000, 0, -1))
        quick_sort(arr)
        assert arr == list(range(1, 1001))


class TestBFS:
    def test_basic(self):
        graph = Graph()
        graph.add_edge(1, 2)
        graph.add_edge(2, 3)
        result = bfs(graph, 1)
        assert len(result) == 3
        assert result[0] == 1

    def test_path(self):
        graph = Graph()
        graph.add_edge(1, 2)
        graph.add_edge(2, 3)
        graph.add_edge(3, 4)
        path = bfs_path(graph, 1, 4)
        assert path is not None
        assert path[0] == 1
        assert path[-1] == 4

    def test_path_not_exists(self):
        graph = Graph()
        graph.add_edge(1, 2)
        graph.add_vertex(3)
        path = bfs_path(graph, 1, 3)
        assert path is None

    def test_distances(self):
        graph = Graph()
        graph.add_edge(1, 2)
        graph.add_edge(1, 3)
        graph.add_edge(2, 4)
        distances = bfs_distances(graph, 1)
        assert distances[1] == 0
        assert distances[2] == 1
        assert distances[4] == 2


class TestDFS:
    def test_basic(self):
        graph = Graph()
        graph.add_edge(1, 2)
        graph.add_edge(2, 3)
        result = dfs(graph, 1)
        assert len(result) == 3
        assert result[0] == 1

    def test_recursive(self):
        graph = Graph()
        graph.add_edge(1, 2)
        graph.add_edge(2, 3)
        result = dfs_recursive(graph, 1)
        assert len(result) == 3

    def test_path(self):
        graph = Graph()
        graph.add_edge(1, 2)
        graph.add_edge(2, 3)
        graph.add_edge(3, 4)
        path = dfs_path(graph, 1, 4)
        assert path is not None
        assert path[0] == 1
        assert path[-1] == 4

    def test_cycle_detection(self):
        graph = Graph(directed=True)
        graph.add_edge(1, 2)
        graph.add_edge(2, 3)
        graph.add_edge(3, 1)
        assert has_cycle(graph, 1)

    def test_no_cycle(self):
        graph = Graph(directed=True)
        graph.add_edge(1, 2)
        graph.add_edge(2, 3)
        assert not has_cycle(graph, 1)
