/// Double-ended queue supporting operations at both ends.
/// Time: O(1) for all operations
/// Space: O(n)
public struct Deque<T> {
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

    public mutating func pushFront(_ value: T) {
        let node = Node(value, next: head)
        if let head = head {
            head.prev = node
        } else {
            tail = node
        }
        head = node
        size += 1
    }

    public mutating func pushBack(_ value: T) {
        let node = Node(value, prev: tail)
        if let tail = tail {
            tail.next = node
        } else {
            head = node
        }
        tail = node
        size += 1
    }

    public mutating func popFront() -> T? {
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

    public mutating func popBack() -> T? {
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

    public func peekFront() -> T? {
        head?.value
    }

    public func peekBack() -> T? {
        tail?.value
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
}
