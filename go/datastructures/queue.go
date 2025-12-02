package datastructures

// Queue implements a FIFO queue using a linked list.
// Time: O(1) for all operations
// Space: O(n)
type Queue[T any] struct {
	head *queueNode[T]
	tail *queueNode[T]
	size int
}

type queueNode[T any] struct {
	value T
	next  *queueNode[T]
}

func NewQueue[T any]() *Queue[T] {
	return &Queue[T]{}
}

func (q *Queue[T]) Enqueue(value T) {
	node := &queueNode[T]{value: value}
	if q.tail != nil {
		q.tail.next = node
	} else {
		q.head = node
	}
	q.tail = node
	q.size++
}

func (q *Queue[T]) Dequeue() (T, bool) {
	var zero T
	if q.head == nil {
		return zero, false
	}
	value := q.head.value
	q.head = q.head.next
	if q.head == nil {
		q.tail = nil
	}
	q.size--
	return value, true
}

func (q *Queue[T]) Peek() (T, bool) {
	var zero T
	if q.head == nil {
		return zero, false
	}
	return q.head.value, true
}

func (q *Queue[T]) IsEmpty() bool {
	return q.size == 0
}

func (q *Queue[T]) Size() int {
	return q.size
}

func (q *Queue[T]) Clear() {
	q.head = nil
	q.tail = nil
	q.size = 0
}
