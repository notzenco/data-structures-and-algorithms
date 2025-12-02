package datastructures

import "cmp"

// MinHeap implements a binary min-heap.
// Time: O(log n) insert/extract, O(1) peek
// Space: O(n)
type MinHeap[T cmp.Ordered] struct {
	heap []T
}

func NewMinHeap[T cmp.Ordered]() *MinHeap[T] {
	return &MinHeap[T]{heap: make([]T, 0)}
}

func (h *MinHeap[T]) Insert(value T) {
	h.heap = append(h.heap, value)
	h.siftUp(len(h.heap) - 1)
}

func (h *MinHeap[T]) ExtractMin() (T, bool) {
	var zero T
	if len(h.heap) == 0 {
		return zero, false
	}

	min := h.heap[0]
	last := h.heap[len(h.heap)-1]
	h.heap = h.heap[:len(h.heap)-1]

	if len(h.heap) > 0 {
		h.heap[0] = last
		h.siftDown(0)
	}

	return min, true
}

func (h *MinHeap[T]) Peek() (T, bool) {
	var zero T
	if len(h.heap) == 0 {
		return zero, false
	}
	return h.heap[0], true
}

func (h *MinHeap[T]) IsEmpty() bool {
	return len(h.heap) == 0
}

func (h *MinHeap[T]) Size() int {
	return len(h.heap)
}

func (h *MinHeap[T]) Clear() {
	h.heap = make([]T, 0)
}

func (h *MinHeap[T]) ToSlice() []T {
	result := make([]T, len(h.heap))
	copy(result, h.heap)
	return result
}

func (h *MinHeap[T]) siftUp(index int) {
	for index > 0 {
		parentIndex := (index - 1) / 2
		if h.heap[index] >= h.heap[parentIndex] {
			break
		}
		h.heap[index], h.heap[parentIndex] = h.heap[parentIndex], h.heap[index]
		index = parentIndex
	}
}

func (h *MinHeap[T]) siftDown(index int) {
	length := len(h.heap)

	for {
		leftChild := 2*index + 1
		rightChild := 2*index + 2
		smallest := index

		if leftChild < length && h.heap[leftChild] < h.heap[smallest] {
			smallest = leftChild
		}

		if rightChild < length && h.heap[rightChild] < h.heap[smallest] {
			smallest = rightChild
		}

		if smallest == index {
			break
		}

		h.heap[index], h.heap[smallest] = h.heap[smallest], h.heap[index]
		index = smallest
	}
}
