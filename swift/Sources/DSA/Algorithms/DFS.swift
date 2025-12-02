/// Depth-First Search algorithm.
/// Time: O(V + E) where V = vertices, E = edges
/// Space: O(V)
public enum DFS {
    /// Perform iterative DFS traversal from a starting vertex.
    public static func traverse<T: Hashable>(_ graph: Graph<T>, from start: T) -> [T] {
        var result: [T] = []
        var visited = Set<T>()

        guard graph.getVertices().contains(start) else { return result }

        var stack = Stack<T>()
        stack.push(start)

        while !stack.isEmpty {
            guard let vertex = stack.pop() else { break }

            if visited.contains(vertex) { continue }

            visited.insert(vertex)
            result.append(vertex)

            // Push neighbors in reverse order to visit them in natural order
            let neighbors = graph.getNeighbors(vertex)
            for neighbor in neighbors.reversed() {
                if !visited.contains(neighbor) {
                    stack.push(neighbor)
                }
            }
        }

        return result
    }

    /// Perform recursive DFS traversal from a starting vertex.
    public static func traverseRecursive<T: Hashable>(_ graph: Graph<T>, from start: T) -> [T] {
        var result: [T] = []
        var visited = Set<T>()

        guard graph.getVertices().contains(start) else { return result }

        dfsRecursive(graph, start, &visited, &result)
        return result
    }

    private static func dfsRecursive<T: Hashable>(_ graph: Graph<T>, _ vertex: T, _ visited: inout Set<T>, _ result: inout [T]) {
        visited.insert(vertex)
        result.append(vertex)

        for neighbor in graph.getNeighbors(vertex) {
            if !visited.contains(neighbor) {
                dfsRecursive(graph, neighbor, &visited, &result)
            }
        }
    }

    /// Find a path between two vertices using DFS.
    public static func findPath<T: Hashable>(_ graph: Graph<T>, from start: T, to end: T) -> [T]? {
        let vertices = graph.getVertices()
        guard vertices.contains(start), vertices.contains(end) else { return nil }
        if start == end { return [start] }

        var visited = Set<T>()
        var parent: [T: T?] = [start: nil]
        var stack = Stack<T>()
        stack.push(start)

        while !stack.isEmpty {
            guard let vertex = stack.pop() else { break }

            if visited.contains(vertex) { continue }
            visited.insert(vertex)

            if vertex == end {
                return reconstructPath(parent, from: start, to: end)
            }

            for neighbor in graph.getNeighbors(vertex) {
                if !visited.contains(neighbor) {
                    parent[neighbor] = vertex
                    stack.push(neighbor)
                }
            }
        }

        return nil
    }

    /// Detect if the graph contains a cycle.
    public static func hasCycle<T: Hashable>(_ graph: Graph<T>) -> Bool {
        var visited = Set<T>()
        var recursionStack = Set<T>()

        for vertex in graph.getVertices() {
            if !visited.contains(vertex) {
                if hasCycleUtil(graph, vertex, &visited, &recursionStack) {
                    return true
                }
            }
        }

        return false
    }

    private static func hasCycleUtil<T: Hashable>(_ graph: Graph<T>, _ vertex: T, _ visited: inout Set<T>, _ recursionStack: inout Set<T>) -> Bool {
        visited.insert(vertex)
        recursionStack.insert(vertex)

        for neighbor in graph.getNeighbors(vertex) {
            if !visited.contains(neighbor) {
                if hasCycleUtil(graph, neighbor, &visited, &recursionStack) {
                    return true
                }
            } else if recursionStack.contains(neighbor) {
                return true
            }
        }

        recursionStack.remove(vertex)
        return false
    }

    /// Perform topological sort on a directed acyclic graph.
    public static func topologicalSort<T: Hashable>(_ graph: Graph<T>) -> [T]? {
        guard graph.directed else { return nil }

        var visited = Set<T>()
        var result: [T] = []
        var recursionStack = Set<T>()

        for vertex in graph.getVertices() {
            if !visited.contains(vertex) {
                if !topologicalSortUtil(graph, vertex, &visited, &result, &recursionStack) {
                    return nil // Cycle detected
                }
            }
        }

        return result.reversed()
    }

    private static func topologicalSortUtil<T: Hashable>(_ graph: Graph<T>, _ vertex: T, _ visited: inout Set<T>, _ result: inout [T], _ recursionStack: inout Set<T>) -> Bool {
        visited.insert(vertex)
        recursionStack.insert(vertex)

        for neighbor in graph.getNeighbors(vertex) {
            if recursionStack.contains(neighbor) {
                return false // Cycle detected
            }
            if !visited.contains(neighbor) {
                if !topologicalSortUtil(graph, neighbor, &visited, &result, &recursionStack) {
                    return false
                }
            }
        }

        recursionStack.remove(vertex)
        result.append(vertex)
        return true
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
