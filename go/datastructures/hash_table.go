package datastructures

import "hash/fnv"

// HashTable implements a hash table with open addressing and linear probing.
// Time: O(1) average for all operations
// Space: O(n)
type HashTable[K comparable, V any] struct {
	buckets  []*hashEntry[K, V]
	size     int
	capacity int
}

type hashEntry[K comparable, V any] struct {
	key     K
	value   V
	deleted bool
}

const loadFactorThreshold = 0.7

func NewHashTable[K comparable, V any](initialCapacity ...int) *HashTable[K, V] {
	cap := 16
	if len(initialCapacity) > 0 && initialCapacity[0] > 0 {
		cap = initialCapacity[0]
	}
	return &HashTable[K, V]{
		buckets:  make([]*hashEntry[K, V], cap),
		capacity: cap,
	}
}

func (h *HashTable[K, V]) Put(key K, value V) {
	if float64(h.size) >= float64(h.capacity)*loadFactorThreshold {
		h.resize(h.capacity * 2)
	}

	index := h.hash(key)
	firstDeleted := -1

	for i := 0; i < h.capacity; i++ {
		entry := h.buckets[index]

		if entry == nil {
			insertIdx := index
			if firstDeleted != -1 {
				insertIdx = firstDeleted
			}
			h.buckets[insertIdx] = &hashEntry[K, V]{key: key, value: value}
			h.size++
			return
		}

		if entry.deleted && firstDeleted == -1 {
			firstDeleted = index
		} else if !entry.deleted && entry.key == key {
			entry.value = value
			return
		}

		index = (index + 1) % h.capacity
	}

	if firstDeleted != -1 {
		h.buckets[firstDeleted] = &hashEntry[K, V]{key: key, value: value}
		h.size++
	}
}

func (h *HashTable[K, V]) Get(key K) (V, bool) {
	var zero V
	index := h.findIndex(key)
	if index == -1 {
		return zero, false
	}
	return h.buckets[index].value, true
}

func (h *HashTable[K, V]) Remove(key K) (V, bool) {
	var zero V
	index := h.findIndex(key)
	if index == -1 {
		return zero, false
	}
	entry := h.buckets[index]
	value := entry.value
	entry.deleted = true
	h.size--
	return value, true
}

func (h *HashTable[K, V]) Contains(key K) bool {
	return h.findIndex(key) != -1
}

func (h *HashTable[K, V]) IsEmpty() bool {
	return h.size == 0
}

func (h *HashTable[K, V]) Size() int {
	return h.size
}

func (h *HashTable[K, V]) Keys() []K {
	result := make([]K, 0, h.size)
	for _, entry := range h.buckets {
		if entry != nil && !entry.deleted {
			result = append(result, entry.key)
		}
	}
	return result
}

func (h *HashTable[K, V]) Values() []V {
	result := make([]V, 0, h.size)
	for _, entry := range h.buckets {
		if entry != nil && !entry.deleted {
			result = append(result, entry.value)
		}
	}
	return result
}

func (h *HashTable[K, V]) Clear() {
	h.buckets = make([]*hashEntry[K, V], h.capacity)
	h.size = 0
}

func (h *HashTable[K, V]) hash(key K) int {
	hasher := fnv.New64a()
	hasher.Write([]byte(any(key).(string)))
	return int(hasher.Sum64() % uint64(h.capacity))
}

func (h *HashTable[K, V]) findIndex(key K) int {
	index := h.hash(key)

	for i := 0; i < h.capacity; i++ {
		entry := h.buckets[index]

		if entry == nil {
			return -1
		}

		if !entry.deleted && entry.key == key {
			return index
		}

		index = (index + 1) % h.capacity
	}

	return -1
}

func (h *HashTable[K, V]) resize(newCapacity int) {
	oldBuckets := h.buckets
	h.buckets = make([]*hashEntry[K, V], newCapacity)
	h.capacity = newCapacity
	h.size = 0

	for _, entry := range oldBuckets {
		if entry != nil && !entry.deleted {
			h.Put(entry.key, entry.value)
		}
	}
}
