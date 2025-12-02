/// Breadth-First Search algorithm.
/// Time: O(V + E) where V = vertices, E = edges
/// Space: O(V)
public enum BFS {
    /// Perform BFS traversal from a starting vertex.
    public static func traverse<T: Hashable>(_ graph: Graph<T>, from start: T) -> [T] {
        var result: [T] = []
        var visited = Set<T>()

        guard graph.getVertices().contains(start) else { return result }

        var queue = Queue<T>()
        queue.enqueue(start)
        visited.insert(start)

        while !queue.isEmpty {
            guard let vertex = queue.dequeue() else { break }
            result.append(vertex)

            for neighbor in graph.getNeighbors(vertex) {
                if !visited.contains(neighbor) {
                    visited.insert(neighbor)
                    queue.enqueue(neighbor)
                }
            }
        }

        return result
    }

    /// Find the shortest path between two vertices using BFS.
    /// Only works for unweighted graphs.
    public static func shortestPath<T: Hashable>(_ graph: Graph<T>, from start: T, to end: T) -> [T]? {
        let vertices = graph.getVertices()
        guard vertices.contains(start), vertices.contains(end) else { return nil }
        if start == end { return [start] }

        var queue = Queue<T>()
        queue.enqueue(start)
        var visited = Set([start])
        var parent: [T: T?] = [start: nil]

        while !queue.isEmpty {
            guard let vertex = queue.dequeue() else { break }

            for neighbor in graph.getNeighbors(vertex) {
                if !visited.contains(neighbor) {
                    visited.insert(neighbor)
                    parent[neighbor] = vertex

                    if neighbor == end {
                        return reconstructPath(parent, from: start, to: end)
                    }

                    queue.enqueue(neighbor)
                }
            }
        }

        return nil
    }

    /// Find distances from start vertex to all reachable vertices.
    public static func distances<T: Hashable>(_ graph: Graph<T>, from start: T) -> [T: Int] {
        var distances: [T: Int] = [:]

        guard graph.getVertices().contains(start) else { return distances }

        var queue = Queue<T>()
        queue.enqueue(start)
        distances[start] = 0

        while !queue.isEmpty {
            guard let vertex = queue.dequeue() else { break }
            let currentDistance = distances[vertex]!

            for neighbor in graph.getNeighbors(vertex) {
                if distances[neighbor] == nil {
                    distances[neighbor] = currentDistance + 1
                    queue.enqueue(neighbor)
                }
            }
        }

        return distances
    }

    private static func reconstructPath<T: Hashable>(_ parent: [T: T?], from start: T, to end: T) -> [T] {
        var path: [T] = []
        var current: T? = end

        while let vertex = current {
            path.insert(vertex, at: 0)
            current = parent[vertex] ?? nil
        }

        return path
    }
}
