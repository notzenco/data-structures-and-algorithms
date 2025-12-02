/// Graph representation using adjacency list.
public class Graph<T: Hashable> {
    private var adjacencyList: [T: [T]] = [:]
    public let directed: Bool

    public init(directed: Bool = false) {
        self.directed = directed
    }

    public func addVertex(_ vertex: T) {
        if adjacencyList[vertex] == nil {
            adjacencyList[vertex] = []
        }
    }

    public func addEdge(from: T, to: T) {
        addVertex(from)
        addVertex(to)

        if !(adjacencyList[from]?.contains(to) ?? false) {
            adjacencyList[from]?.append(to)
        }

        if !directed && !(adjacencyList[to]?.contains(from) ?? false) {
            adjacencyList[to]?.append(from)
        }
    }

    public func removeEdge(from: T, to: T) {
        adjacencyList[from]?.removeAll { $0 == to }
        if !directed {
            adjacencyList[to]?.removeAll { $0 == from }
        }
    }

    public func removeVertex(_ vertex: T) {
        adjacencyList.removeValue(forKey: vertex)
        for key in adjacencyList.keys {
            adjacencyList[key]?.removeAll { $0 == vertex }
        }
    }

    public func hasEdge(from: T, to: T) -> Bool {
        adjacencyList[from]?.contains(to) ?? false
    }

    public func getNeighbors(_ vertex: T) -> [T] {
        adjacencyList[vertex] ?? []
    }

    public func getVertices() -> Set<T> {
        Set(adjacencyList.keys)
    }

    public var vertexCount: Int {
        adjacencyList.count
    }
}
