/// Binary min-heap implementation.
/// Time: O(log n) insert/extract, O(1) peek
/// Space: O(n)
public struct MinHeap<T: Comparable> {
    private var heap: [T] = []
    private let comparator: (T, T) -> Int

    public init(comparator: ((T, T) -> Int)? = nil) {
        self.comparator = comparator ?? { a, b in
            if a < b { return -1 }
            if a > b { return 1 }
            return 0
        }
    }

    public mutating func insert(_ value: T) {
        heap.append(value)
        siftUp(heap.count - 1)
    }

    public mutating func extractMin() -> T? {
        guard !heap.isEmpty else { return nil }

        let min = heap[0]
        let last = heap.removeLast()

        if !heap.isEmpty {
            heap[0] = last
            siftDown(0)
        }

        return min
    }

    public func peek() -> T? {
        heap.first
    }

    public var isEmpty: Bool {
        heap.isEmpty
    }

    public var count: Int {
        heap.count
    }

    public mutating func clear() {
        heap.removeAll()
    }

    public func toArray() -> [T] {
        heap
    }

    private mutating func siftUp(_ index: Int) {
        var i = index
        while i > 0 {
            let parentIndex = (i - 1) / 2
            if comparator(heap[i], heap[parentIndex]) >= 0 { break }
            heap.swapAt(i, parentIndex)
            i = parentIndex
        }
    }

    private mutating func siftDown(_ index: Int) {
        var i = index

        while true {
            let leftChild = 2 * i + 1
            let rightChild = 2 * i + 2
            var smallest = i

            if leftChild < heap.count && comparator(heap[leftChild], heap[smallest]) < 0 {
                smallest = leftChild
            }

            if rightChild < heap.count && comparator(heap[rightChild], heap[smallest]) < 0 {
                smallest = rightChild
            }

            if smallest == i { break }

            heap.swapAt(i, smallest)
            i = smallest
        }
    }
}
