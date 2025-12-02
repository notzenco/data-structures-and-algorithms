namespace DSA.Algorithms;

/// <summary>
/// Depth-First Search implementation.
/// Time: O(V + E)
/// Space: O(V)
/// </summary>
public static class DFS
{
    public static List<T> Traverse<T>(Graph<T> graph, T start, Action<T>? callback = null) where T : notnull
    {
        var result = new List<T>();

        if (!graph.HasVertex(start))
            return result;

        var visited = new HashSet<T>();
        var stack = new Stack<T>();

        stack.Push(start);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            if (visited.Contains(current))
                continue;

            visited.Add(current);
            result.Add(current);
            callback?.Invoke(current);

            var neighbors = graph.Neighbors(current).ToList();
            for (int i = neighbors.Count - 1; i >= 0; i--)
            {
                if (!visited.Contains(neighbors[i]))
                    stack.Push(neighbors[i]);
            }
        }

        return result;
    }

    public static List<T> TraverseRecursive<T>(Graph<T> graph, T start, Action<T>? callback = null) where T : notnull
    {
        var result = new List<T>();

        if (!graph.HasVertex(start))
            return result;

        var visited = new HashSet<T>();
        DfsRecursive(graph, start, visited, result, callback);
        return result;
    }

    private static void DfsRecursive<T>(
        Graph<T> graph, T current, HashSet<T> visited, List<T> result, Action<T>? callback) where T : notnull
    {
        visited.Add(current);
        result.Add(current);
        callback?.Invoke(current);

        foreach (var neighbor in graph.Neighbors(current))
        {
            if (!visited.Contains(neighbor))
                DfsRecursive(graph, neighbor, visited, result, callback);
        }
    }

    public static List<T>? FindPath<T>(Graph<T> graph, T start, T end) where T : notnull
    {
        if (!graph.HasVertex(start) || !graph.HasVertex(end))
            return null;

        if (EqualityComparer<T>.Default.Equals(start, end))
            return new List<T> { start };

        var visited = new HashSet<T>();
        var path = new List<T>();

        if (DfsPath(graph, start, end, visited, path))
            return path;
        return null;
    }

    private static bool DfsPath<T>(
        Graph<T> graph, T current, T end, HashSet<T> visited, List<T> path) where T : notnull
    {
        visited.Add(current);
        path.Add(current);

        if (EqualityComparer<T>.Default.Equals(current, end))
            return true;

        foreach (var neighbor in graph.Neighbors(current))
        {
            if (!visited.Contains(neighbor))
            {
                if (DfsPath(graph, neighbor, end, visited, path))
                    return true;
            }
        }

        path.RemoveAt(path.Count - 1);
        return false;
    }

    public static bool HasCycle<T>(Graph<T> graph, T start) where T : notnull
    {
        if (!graph.HasVertex(start))
            return false;

        var visited = new HashSet<T>();
        var recStack = new HashSet<T>();

        return DetectCycle(graph, start, visited, recStack);
    }

    private static bool DetectCycle<T>(
        Graph<T> graph, T current, HashSet<T> visited, HashSet<T> recStack) where T : notnull
    {
        visited.Add(current);
        recStack.Add(current);

        foreach (var neighbor in graph.Neighbors(current))
        {
            if (!visited.Contains(neighbor))
            {
                if (DetectCycle(graph, neighbor, visited, recStack))
                    return true;
            }
            else if (recStack.Contains(neighbor))
            {
                return true;
            }
        }

        recStack.Remove(current);
        return false;
    }
}
