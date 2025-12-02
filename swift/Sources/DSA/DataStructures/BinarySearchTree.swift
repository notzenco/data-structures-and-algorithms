/// Binary Search Tree implementation.
/// Time: O(log n) average, O(n) worst for all operations
/// Space: O(n)
public struct BinarySearchTree<T: Comparable> {
    private class Node {
        var value: T
        var left: Node?
        var right: Node?

        init(_ value: T) {
            self.value = value
        }
    }

    private var root: Node?
    private var size = 0
    private let comparator: (T, T) -> Int

    public init(comparator: ((T, T) -> Int)? = nil) {
        self.comparator = comparator ?? { a, b in
            if a < b { return -1 }
            if a > b { return 1 }
            return 0
        }
    }

    public mutating func insert(_ value: T) {
        root = insertNode(root, value)
        size += 1
    }

    private func insertNode(_ node: Node?, _ value: T) -> Node {
        guard let node = node else { return Node(value) }

        let cmp = comparator(value, node.value)
        if cmp < 0 {
            node.left = insertNode(node.left, value)
        } else if cmp > 0 {
            node.right = insertNode(node.right, value)
        }
        return node
    }

    public func contains(_ value: T) -> Bool {
        findNode(root, value) != nil
    }

    private func findNode(_ node: Node?, _ value: T) -> Node? {
        guard let node = node else { return nil }

        let cmp = comparator(value, node.value)
        if cmp < 0 {
            return findNode(node.left, value)
        } else if cmp > 0 {
            return findNode(node.right, value)
        }
        return node
    }

    public mutating func remove(_ value: T) -> Bool {
        let sizeBefore = size
        root = removeNode(root, value)
        return size < sizeBefore
    }

    private func removeNode(_ node: Node?, _ value: T) -> Node? {
        guard let node = node else { return nil }

        let cmp = comparator(value, node.value)
        if cmp < 0 {
            node.left = removeNode(node.left, value)
        } else if cmp > 0 {
            node.right = removeNode(node.right, value)
        } else {
            size -= 1

            if node.left == nil {
                return node.right
            }
            if node.right == nil {
                return node.left
            }

            let minRight = findMinNode(node.right!)
            node.value = minRight.value
            node.right = removeNode(node.right, minRight.value)
            size += 1 // Compensate for recursive call
        }
        return node
    }

    private func findMinNode(_ node: Node) -> Node {
        var current = node
        while let left = current.left {
            current = left
        }
        return current
    }

    private func findMaxNode(_ node: Node) -> Node {
        var current = node
        while let right = current.right {
            current = right
        }
        return current
    }

    public func min() -> T? {
        guard let root = root else { return nil }
        return findMinNode(root).value
    }

    public func max() -> T? {
        guard let root = root else { return nil }
        return findMaxNode(root).value
    }

    public func inOrder() -> [T] {
        var result: [T] = []
        inOrderTraverse(root, &result)
        return result
    }

    private func inOrderTraverse(_ node: Node?, _ result: inout [T]) {
        guard let node = node else { return }
        inOrderTraverse(node.left, &result)
        result.append(node.value)
        inOrderTraverse(node.right, &result)
    }

    public var isEmpty: Bool {
        size == 0
    }

    public var count: Int {
        size
    }

    public mutating func clear() {
        root = nil
        size = 0
    }
}
