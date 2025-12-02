/// Disjoint Set (Union-Find) implementation with path compression and union by rank.
/// Time: O(α(n)) amortized for all operations (nearly constant)
/// Space: O(n)
public struct DisjointSet {
    private var parent: [Int]
    private var rank: [Int]
    private var setCount: Int

    public init(_ size: Int) {
        parent = Array(0..<size)
        rank = Array(repeating: 0, count: size)
        setCount = size
    }

    public mutating func find(_ x: Int) -> Int {
        if parent[x] != x {
            parent[x] = find(parent[x]) // Path compression
        }
        return parent[x]
    }

    public mutating func union(_ x: Int, _ y: Int) -> Bool {
        let rootX = find(x)
        let rootY = find(y)

        if rootX == rootY { return false }

        // Union by rank
        if rank[rootX] < rank[rootY] {
            parent[rootX] = rootY
        } else if rank[rootX] > rank[rootY] {
            parent[rootY] = rootX
        } else {
            parent[rootY] = rootX
            rank[rootX] += 1
        }

        setCount -= 1
        return true
    }

    public mutating func connected(_ x: Int, _ y: Int) -> Bool {
        find(x) == find(y)
    }

    public var count: Int {
        setCount
    }

    public var size: Int {
        parent.count
    }
}
