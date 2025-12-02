using DSA.Algorithms;
using Xunit;

namespace DSA.Tests;

public class BinarySearchTests
{
    [Fact]
    public void FindsElement()
    {
        var arr = new[] { 1, 3, 5, 7, 9, 11, 13 };
        Assert.Equal(0, BinarySearch.Search(arr, 1));
        Assert.Equal(3, BinarySearch.Search(arr, 7));
        Assert.Equal(6, BinarySearch.Search(arr, 13));
    }

    [Fact]
    public void ElementNotFound()
    {
        var arr = new[] { 1, 3, 5, 7, 9 };
        Assert.Null(BinarySearch.Search(arr, 0));
        Assert.Null(BinarySearch.Search(arr, 4));
        Assert.Null(BinarySearch.Search(arr, 10));
    }

    [Fact]
    public void LowerBound()
    {
        var arr = new[] { 1, 2, 2, 2, 3, 4, 5 };
        Assert.Equal(1, BinarySearch.LowerBound(arr, 2));
        Assert.Equal(4, BinarySearch.LowerBound(arr, 3));
    }

    [Fact]
    public void UpperBound()
    {
        var arr = new[] { 1, 2, 2, 2, 3, 4, 5 };
        Assert.Equal(4, BinarySearch.UpperBound(arr, 2));
        Assert.Equal(5, BinarySearch.UpperBound(arr, 3));
    }
}

public class InsertionSortTests
{
    [Fact]
    public void SortsArray()
    {
        var arr = new List<int> { 5, 2, 8, 1, 9, 3 };
        InsertionSort.Sort(arr);
        Assert.Equal(new[] { 1, 2, 3, 5, 8, 9 }, arr);
    }

    [Fact]
    public void SortsDescending()
    {
        var arr = new List<int> { 1, 5, 3, 2, 4 };
        InsertionSort.SortDescending(arr);
        Assert.Equal(new[] { 5, 4, 3, 2, 1 }, arr);
    }
}

public class MergeSortTests
{
    [Fact]
    public void SortsArray()
    {
        var arr = new List<int> { 5, 2, 8, 1, 9, 3 };
        MergeSort.Sort(arr);
        Assert.Equal(new[] { 1, 2, 3, 5, 8, 9 }, arr);
    }

    [Fact]
    public void SortsWithDuplicates()
    {
        var arr = new List<int> { 3, 1, 4, 1, 5, 9, 2, 6, 5, 3 };
        MergeSort.Sort(arr);
        Assert.Equal(new[] { 1, 1, 2, 3, 3, 4, 5, 5, 6, 9 }, arr);
    }
}

public class QuickSortTests
{
    [Fact]
    public void SortsArray()
    {
        var arr = new List<int> { 5, 2, 8, 1, 9, 3 };
        QuickSort.Sort(arr);
        Assert.Equal(new[] { 1, 2, 3, 5, 8, 9 }, arr);
    }

    [Fact]
    public void SortsLargeArray()
    {
        var rand = new Random(42);
        var arr = new List<int>();
        for (int i = 0; i < 1000; i++)
            arr.Add(rand.Next(1000));

        var expected = arr.OrderBy(x => x).ToList();
        QuickSort.Sort(arr);
        Assert.Equal(expected, arr);
    }
}

public class BFSTests
{
    [Fact]
    public void Traversal()
    {
        var graph = new Graph<string>();
        graph.AddEdge("A", "B");
        graph.AddEdge("A", "C");
        graph.AddEdge("B", "D");
        graph.AddEdge("C", "D");

        var result = BFS.Traverse(graph, "A");
        Assert.Equal("A", result[0]);
        Assert.Contains("B", result);
        Assert.Contains("C", result);
        Assert.Contains("D", result);
        Assert.Equal(4, result.Count);
    }

    [Fact]
    public void FindPath()
    {
        var graph = new Graph<string>();
        graph.AddEdge("A", "B");
        graph.AddEdge("B", "C");
        graph.AddEdge("C", "D");
        graph.AddEdge("A", "D");

        var path = BFS.FindPath(graph, "A", "D");
        Assert.NotNull(path);
        Assert.Equal("A", path![0]);
        Assert.Equal("D", path[^1]);
        Assert.Equal(2, path.Count); // Shortest path
    }

    [Fact]
    public void Distances()
    {
        var graph = new Graph<string>();
        graph.AddEdge("A", "B");
        graph.AddEdge("A", "C");
        graph.AddEdge("B", "D");
        graph.AddEdge("D", "E");

        var distances = BFS.Distances(graph, "A");
        Assert.Equal(0, distances["A"]);
        Assert.Equal(1, distances["B"]);
        Assert.Equal(1, distances["C"]);
        Assert.Equal(2, distances["D"]);
        Assert.Equal(3, distances["E"]);
    }
}

public class DFSTests
{
    [Fact]
    public void Traversal()
    {
        var graph = new Graph<string>();
        graph.AddEdge("A", "B");
        graph.AddEdge("A", "C");
        graph.AddEdge("B", "D");

        var result = DFS.Traverse(graph, "A");
        Assert.Equal("A", result[0]);
        Assert.Equal(4, result.Count);
    }

    [Fact]
    public void FindPath()
    {
        var graph = new Graph<string>();
        graph.AddEdge("A", "B");
        graph.AddEdge("B", "C");
        graph.AddEdge("C", "D");

        var path = DFS.FindPath(graph, "A", "D");
        Assert.NotNull(path);
        Assert.Equal("A", path![0]);
        Assert.Equal("D", path[^1]);
    }

    [Fact]
    public void CycleDetection()
    {
        var graph = new Graph<string>(directed: true);
        graph.AddEdge("A", "B");
        graph.AddEdge("B", "C");
        graph.AddEdge("C", "A");

        Assert.True(DFS.HasCycle(graph, "A"));
    }

    [Fact]
    public void NoCycle()
    {
        var graph = new Graph<string>(directed: true);
        graph.AddEdge("A", "B");
        graph.AddEdge("B", "C");
        graph.AddEdge("A", "C");

        Assert.False(DFS.HasCycle(graph, "A"));
    }
}
