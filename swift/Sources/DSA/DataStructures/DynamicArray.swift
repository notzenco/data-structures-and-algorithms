/// Resizable array with automatic capacity management.
/// Time: O(1) amortized for append, O(n) for insert/remove
/// Space: O(n)
public struct DynamicArray<T> {
    private var items: [T] = []

    public init() {}

    public mutating func push(_ value: T) {
        items.append(value)
    }

    public mutating func pop() -> T? {
        items.popLast()
    }

    public func get(_ index: Int) -> T? {
        guard index >= 0 && index < items.count else { return nil }
        return items[index]
    }

    public mutating func set(_ index: Int, _ value: T) -> Bool {
        guard index >= 0 && index < items.count else { return false }
        items[index] = value
        return true
    }

    public mutating func insert(_ index: Int, _ value: T) -> Bool {
        guard index >= 0 && index <= items.count else { return false }
        items.insert(value, at: index)
        return true
    }

    public mutating func removeAt(_ index: Int) -> T? {
        guard index >= 0 && index < items.count else { return nil }
        return items.remove(at: index)
    }

    public func indexOf(_ value: T) -> Int where T: Equatable {
        items.firstIndex(of: value) ?? -1
    }

    public func contains(_ value: T) -> Bool where T: Equatable {
        items.contains(value)
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
