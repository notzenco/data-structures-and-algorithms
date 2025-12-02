/// Singly linked list with forward traversal.
/// Time: O(1) for prepend, O(n) for other operations
/// Space: O(n)
public struct SinglyLinkedList<T> {
    private class Node {
        var value: T
        var next: Node?

        init(_ value: T, next: Node? = nil) {
            self.value = value
            self.next = next
        }
    }

    private var head: Node?
    private var size = 0

    public init() {}

    public mutating func prepend(_ value: T) {
        head = Node(value, next: head)
        size += 1
    }

    public mutating func append(_ value: T) {
        let node = Node(value)
        if head == nil {
            head = node
        } else {
            var current = head
            while current?.next != nil {
                current = current?.next
            }
            current?.next = node
        }
        size += 1
    }

    public mutating func insert(_ index: Int, _ value: T) -> Bool {
        guard index >= 0 && index <= size else { return false }
        if index == 0 {
            prepend(value)
            return true
        }
        var current = head
        for _ in 0..<(index - 1) {
            current = current?.next
        }
        current?.next = Node(value, next: current?.next)
        size += 1
        return true
    }

    public mutating func removeAt(_ index: Int) -> T? {
        guard index >= 0 && index < size else { return nil }
        if index == 0 {
            let value = head?.value
            head = head?.next
            size -= 1
            return value
        }
        var current = head
        for _ in 0..<(index - 1) {
            current = current?.next
        }
        let value = current?.next?.value
        current?.next = current?.next?.next
        size -= 1
        return value
    }

    public func get(_ index: Int) -> T? {
        guard index >= 0 && index < size else { return nil }
        var current = head
        for _ in 0..<index {
            current = current?.next
        }
        return current?.value
    }

    public func indexOf(_ value: T) -> Int where T: Equatable {
        var current = head
        var index = 0
        while let node = current {
            if node.value == value { return index }
            current = node.next
            index += 1
        }
        return -1
    }

    public func contains(_ value: T) -> Bool where T: Equatable {
        indexOf(value) != -1
    }

    public var isEmpty: Bool {
        size == 0
    }

    public var count: Int {
        size
    }

    public mutating func clear() {
        head = nil
        size = 0
    }

    public func toArray() -> [T] {
        var result: [T] = []
        var current = head
        while let node = current {
            result.append(node.value)
            current = node.next
        }
        return result
    }
}
