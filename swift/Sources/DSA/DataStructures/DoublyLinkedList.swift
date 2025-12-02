/// Doubly linked list with bidirectional traversal.
/// Time: O(1) for prepend/append/removeFirst/removeLast, O(n) for arbitrary access
/// Space: O(n)
public struct DoublyLinkedList<T> {
    private class Node {
        var value: T
        var prev: Node?
        var next: Node?

        init(_ value: T, prev: Node? = nil, next: Node? = nil) {
            self.value = value
            self.prev = prev
            self.next = next
        }
    }

    private var head: Node?
    private var tail: Node?
    private var size = 0

    public init() {}

    public mutating func prepend(_ value: T) {
        let node = Node(value, next: head)
        if let head = head {
            head.prev = node
        } else {
            tail = node
        }
        head = node
        size += 1
    }

    public mutating func append(_ value: T) {
        let node = Node(value, prev: tail)
        if let tail = tail {
            tail.next = node
        } else {
            head = node
        }
        tail = node
        size += 1
    }

    public mutating func removeFirst() -> T? {
        guard let node = head else { return nil }
        head = node.next
        if let head = head {
            head.prev = nil
        } else {
            tail = nil
        }
        size -= 1
        return node.value
    }

    public mutating func removeLast() -> T? {
        guard let node = tail else { return nil }
        tail = node.prev
        if let tail = tail {
            tail.next = nil
        } else {
            head = nil
        }
        size -= 1
        return node.value
    }

    public func getFirst() -> T? {
        head?.value
    }

    public func getLast() -> T? {
        tail?.value
    }

    public func get(_ index: Int) -> T? {
        guard index >= 0 && index < size else { return nil }

        let node: Node?
        if index < size / 2 {
            node = traverseForward(to: index)
        } else {
            node = traverseBackward(to: index)
        }
        return node?.value
    }

    private func traverseForward(to index: Int) -> Node? {
        var current = head
        for _ in 0..<index {
            current = current?.next
        }
        return current
    }

    private func traverseBackward(to index: Int) -> Node? {
        var current = tail
        for _ in stride(from: size - 1, to: index, by: -1) {
            current = current?.prev
        }
        return current
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
        tail = nil
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
