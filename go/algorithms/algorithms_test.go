package algorithms

import (
	"reflect"
	"testing"
)

func TestBinarySearch(t *testing.T) {
	arr := []int{1, 3, 5, 7, 9, 11, 13}

	if idx := BinarySearch(arr, 7); idx != 3 {
		t.Errorf("Expected index 3, got %d", idx)
	}

	if idx := BinarySearch(arr, 4); idx != -1 {
		t.Errorf("Expected index -1, got %d", idx)
	}
}

func TestLowerUpperBound(t *testing.T) {
	arr := []int{1, 2, 2, 2, 3, 4, 5}

	if idx := LowerBound(arr, 2); idx != 1 {
		t.Errorf("LowerBound: expected 1, got %d", idx)
	}

	if idx := UpperBound(arr, 2); idx != 4 {
		t.Errorf("UpperBound: expected 4, got %d", idx)
	}
}

func TestInsertionSort(t *testing.T) {
	arr := []int{5, 2, 8, 1, 9, 3}
	InsertionSort(arr)
	expected := []int{1, 2, 3, 5, 8, 9}

	if !reflect.DeepEqual(arr, expected) {
		t.Errorf("Expected %v, got %v", expected, arr)
	}
}

func TestMergeSort(t *testing.T) {
	arr := []int{5, 2, 8, 1, 9, 3}
	MergeSort(arr)
	expected := []int{1, 2, 3, 5, 8, 9}

	if !reflect.DeepEqual(arr, expected) {
		t.Errorf("Expected %v, got %v", expected, arr)
	}
}

func TestQuickSort(t *testing.T) {
	arr := []int{5, 2, 8, 1, 9, 3}
	QuickSort(arr)
	expected := []int{1, 2, 3, 5, 8, 9}

	if !reflect.DeepEqual(arr, expected) {
		t.Errorf("Expected %v, got %v", expected, arr)
	}
}

func TestQuickSortDuplicates(t *testing.T) {
	arr := []int{3, 1, 4, 1, 5, 9, 2, 6, 5, 3}
	QuickSort(arr)
	expected := []int{1, 1, 2, 3, 3, 4, 5, 5, 6, 9}

	if !reflect.DeepEqual(arr, expected) {
		t.Errorf("Expected %v, got %v", expected, arr)
	}
}

func TestBFS(t *testing.T) {
	graph := NewGraph[string](false)
	graph.AddEdge("A", "B")
	graph.AddEdge("A", "C")
	graph.AddEdge("B", "D")
	graph.AddEdge("C", "D")

	result := BFS(graph, "A", nil)

	if result[0] != "A" {
		t.Errorf("Expected first element A, got %s", result[0])
	}
	if len(result) != 4 {
		t.Errorf("Expected 4 elements, got %d", len(result))
	}
}

func TestBFSFindPath(t *testing.T) {
	graph := NewGraph[string](false)
	graph.AddEdge("A", "B")
	graph.AddEdge("B", "C")
	graph.AddEdge("C", "D")
	graph.AddEdge("A", "D")

	path := BFSFindPath(graph, "A", "D")

	if path == nil {
		t.Error("Expected path, got nil")
	}
	if path[0] != "A" || path[len(path)-1] != "D" {
		t.Errorf("Path should start with A and end with D")
	}
	if len(path) != 2 {
		t.Errorf("Expected shortest path length 2, got %d", len(path))
	}
}

func TestBFSDistances(t *testing.T) {
	graph := NewGraph[string](false)
	graph.AddEdge("A", "B")
	graph.AddEdge("A", "C")
	graph.AddEdge("B", "D")
	graph.AddEdge("D", "E")

	distances := BFSDistances(graph, "A")

	if distances["A"] != 0 {
		t.Errorf("Distance to A should be 0")
	}
	if distances["B"] != 1 {
		t.Errorf("Distance to B should be 1")
	}
	if distances["D"] != 2 {
		t.Errorf("Distance to D should be 2")
	}
	if distances["E"] != 3 {
		t.Errorf("Distance to E should be 3")
	}
}

func TestDFS(t *testing.T) {
	graph := NewGraph[string](false)
	graph.AddEdge("A", "B")
	graph.AddEdge("A", "C")
	graph.AddEdge("B", "D")

	result := DFS(graph, "A", nil)

	if result[0] != "A" {
		t.Errorf("Expected first element A, got %s", result[0])
	}
	if len(result) != 4 {
		t.Errorf("Expected 4 elements, got %d", len(result))
	}
}

func TestDFSFindPath(t *testing.T) {
	graph := NewGraph[string](false)
	graph.AddEdge("A", "B")
	graph.AddEdge("B", "C")
	graph.AddEdge("C", "D")

	path := DFSFindPath(graph, "A", "D")

	if path == nil {
		t.Error("Expected path, got nil")
	}
	if path[0] != "A" || path[len(path)-1] != "D" {
		t.Errorf("Path should start with A and end with D")
	}
}

func TestHasCycle(t *testing.T) {
	graph := NewGraph[string](true)
	graph.AddEdge("A", "B")
	graph.AddEdge("B", "C")
	graph.AddEdge("C", "A")

	if !HasCycle(graph, "A") {
		t.Error("Expected cycle to be detected")
	}

	graph2 := NewGraph[string](true)
	graph2.AddEdge("A", "B")
	graph2.AddEdge("B", "C")
	graph2.AddEdge("A", "C")

	if HasCycle(graph2, "A") {
		t.Error("No cycle should be detected")
	}
}
