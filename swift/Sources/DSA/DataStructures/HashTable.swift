/// Hash table implementation with open addressing and linear probing.
/// Time: O(1) average for all operations
/// Space: O(n)
public struct HashTable<K: Hashable, V> {
    private struct Entry {
        let key: K
        var value: V
        var deleted: Bool = false
    }

    private var buckets: [Entry?]
    private var size = 0
    private let loadFactor = 0.7

    public init(capacity: Int = 16) {
        buckets = Array(repeating: nil, count: max(1, capacity))
    }

    public mutating func put(_ key: K, _ value: V) {
        if Double(size) >= Double(buckets.count) * loadFactor {
            resize(buckets.count * 2)
        }

        var index = hash(key)
        var firstDeleted = -1

        for _ in 0..<buckets.count {
            if let entry = buckets[index] {
                if entry.deleted && firstDeleted == -1 {
                    firstDeleted = index
                } else if !entry.deleted && entry.key == key {
                    buckets[index]?.value = value
                    return
                }
            } else {
                let insertIndex = firstDeleted != -1 ? firstDeleted : index
                buckets[insertIndex] = Entry(key: key, value: value)
                size += 1
                return
            }
            index = (index + 1) % buckets.count
        }

        if firstDeleted != -1 {
            buckets[firstDeleted] = Entry(key: key, value: value)
            size += 1
        }
    }

    public func get(_ key: K) -> V? {
        if let index = findIndex(key) {
            return buckets[index]?.value
        }
        return nil
    }

    public mutating func remove(_ key: K) -> V? {
        if let index = findIndex(key) {
            let value = buckets[index]?.value
            buckets[index]?.deleted = true
            size -= 1
            return value
        }
        return nil
    }

    public func contains(_ key: K) -> Bool {
        findIndex(key) != nil
    }

    public var isEmpty: Bool {
        size == 0
    }

    public var count: Int {
        size
    }

    public func keys() -> [K] {
        buckets.compactMap { entry in
            guard let entry = entry, !entry.deleted else { return nil }
            return entry.key
        }
    }

    public func values() -> [V] {
        buckets.compactMap { entry in
            guard let entry = entry, !entry.deleted else { return nil }
            return entry.value
        }
    }

    public mutating func clear() {
        buckets = Array(repeating: nil, count: buckets.count)
        size = 0
    }

    private func hash(_ key: K) -> Int {
        abs(key.hashValue) % buckets.count
    }

    private func findIndex(_ key: K) -> Int? {
        var index = hash(key)

        for _ in 0..<buckets.count {
            guard let entry = buckets[index] else { return nil }
            if !entry.deleted && entry.key == key {
                return index
            }
            index = (index + 1) % buckets.count
        }

        return nil
    }

    private mutating func resize(_ newCapacity: Int) {
        let oldBuckets = buckets
        buckets = Array(repeating: nil, count: newCapacity)
        size = 0

        for entry in oldBuckets {
            if let entry = entry, !entry.deleted {
                put(entry.key, entry.value)
            }
        }
    }
}
