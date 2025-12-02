namespace DSA.Algorithms;

/// <summary>
/// Breadth-First Search implementation.
/// Time: O(V + E)
/// Space: O(V)
/// </summary>
public static class BFS
{
    public static List<T> Traverse<T>(Graph<T> graph, T start, Action<T>? callback = null) where T : notnull
    {
        var result = new List<T>();

        if (!graph.HasVertex(start))
            return result;

        var visited = new HashSet<T>();
        var queue = new Queue<T>();

        queue.Enqueue(start);
        visited.Add(start);

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();
            result.Add(current);
            callback?.Invoke(current);

            foreach (var neighbor in graph.Neighbors(current))
            {
                if (!visited.Contains(neighbor))
                {
                    visited.Add(neighbor);
                    queue.Enqueue(neighbor);
                }
            }
        }

        return result;
    }

    public static List<T>? FindPath<T>(Graph<T> graph, T start, T end) where T : notnull
    {
        if (!graph.HasVertex(start) || !graph.HasVertex(end))
            return null;

        if (EqualityComparer<T>.Default.Equals(start, end))
            return new List<T> { start };

        var visited = new HashSet<T>();
        var parent = new Dictionary<T, T>();
        var queue = new Queue<T>();

        queue.Enqueue(start);
        visited.Add(start);

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (EqualityComparer<T>.Default.Equals(current, end))
            {
                var path = new List<T>();
                var node = end;
                while (!EqualityComparer<T>.Default.Equals(node, start))
                {
                    path.Add(node);
                    node = parent[node];
                }
                path.Add(start);
                path.Reverse();
                return path;
            }

            foreach (var neighbor in graph.Neighbors(current))
            {
                if (!visited.Contains(neighbor))
                {
                    visited.Add(neighbor);
                    parent[neighbor] = current;
                    queue.Enqueue(neighbor);
                }
            }
        }

        return null;
    }

    public static Dictionary<T, int> Distances<T>(Graph<T> graph, T start) where T : notnull
    {
        var distances = new Dictionary<T, int>();

        if (!graph.HasVertex(start))
            return distances;

        var visited = new HashSet<T>();
        var queue = new Queue<(T vertex, int distance)>();

        queue.Enqueue((start, 0));
        visited.Add(start);
        distances[start] = 0;

        while (queue.Count > 0)
        {
            var (current, dist) = queue.Dequeue();

            foreach (var neighbor in graph.Neighbors(current))
            {
                if (!visited.Contains(neighbor))
                {
                    visited.Add(neighbor);
                    distances[neighbor] = dist + 1;
                    queue.Enqueue((neighbor, dist + 1));
                }
            }
        }

        return distances;
    }
}
