/// LIFO (Last In, First Out) data structure.
/// Time: O(1) for all operations
/// Space: O(n)
public struct Stack<T> {
    private var items: [T] = []

    public init() {}

    public mutating func push(_ value: T) {
        items.append(value)
    }

    public mutating func pop() -> T? {
        items.popLast()
    }

    public func peek() -> T? {
        items.last
    }

    public var isEmpty: Bool {
        items.isEmpty
    }

    public var count: Int {
        items.count
    }

    public mutating func clear() {
        items.removeAll()
    }

    public func toArray() -> [T] {
        items
    }
}
