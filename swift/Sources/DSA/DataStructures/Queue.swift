/// FIFO (First In, First Out) data structure using a linked list.
/// Time: O(1) for all operations
/// Space: O(n)
public struct Queue<T> {
    private class Node {
        var value: T
        var next: Node?

        init(_ value: T) {
            self.value = value
        }
    }

    private var head: Node?
    private var tail: Node?
    private var size = 0

    public init() {}

    public mutating func enqueue(_ value: T) {
        let node = Node(value)
        if let tail = tail {
            tail.next = node
        } else {
            head = node
        }
        tail = node
        size += 1
    }

    public mutating func dequeue() -> T? {
        guard let node = head else { return nil }
        head = node.next
        if head == nil {
            tail = nil
        }
        size -= 1
        return node.value
    }

    public func peek() -> T? {
        head?.value
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
