namespace DSA.Algorithms;

/// <summary>
/// Graph implementation using adjacency list.
/// </summary>
public class Graph<T> where T : notnull
{
    private readonly Dictionary<T, List<T>> _adjList = new();
    private readonly bool _directed;

    public bool IsDirected => _directed;
    public int Count => _adjList.Count;

    public Graph(bool directed = false)
    {
        _directed = directed;
    }

    public void AddVertex(T vertex)
    {
        if (!_adjList.ContainsKey(vertex))
            _adjList[vertex] = new List<T>();
    }

    public void AddEdge(T from, T to)
    {
        AddVertex(from);
        AddVertex(to);
        _adjList[from].Add(to);
        if (!_directed)
            _adjList[to].Add(from);
    }

    public IEnumerable<T> Neighbors(T vertex)
    {
        return _adjList.TryGetValue(vertex, out var neighbors)
            ? neighbors
            : Enumerable.Empty<T>();
    }

    public bool HasVertex(T vertex) => _adjList.ContainsKey(vertex);

    public IEnumerable<T> Vertices => _adjList.Keys;
}
