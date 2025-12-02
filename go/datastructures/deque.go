package datastructures

// Deque implements a double-ended queue using a doubly linked list.
// Time: O(1) for all operations
// Space: O(n)
type Deque[T any] struct {
	head *dequeNode[T]
	tail *dequeNode[T]
	size int
}

type dequeNode[T any] struct {
	value T
	prev  *dequeNode[T]
	next  *dequeNode[T]
}

func NewDeque[T any]() *Deque[T] {
	return &Deque[T]{}
}

func (d *Deque[T]) PushFront(value T) {
	node := &dequeNode[T]{value: value, next: d.head}
	if d.head != nil {
		d.head.prev = node
	} else {
		d.tail = node
	}
	d.head = node
	d.size++
}

func (d *Deque[T]) PushBack(value T) {
	node := &dequeNode[T]{value: value, prev: d.tail}
	if d.tail != nil {
		d.tail.next = node
	} else {
		d.head = node
	}
	d.tail = node
	d.size++
}

func (d *Deque[T]) PopFront() (T, bool) {
	var zero T
	if d.head == nil {
		return zero, false
	}
	value := d.head.value
	d.head = d.head.next
	if d.head != nil {
		d.head.prev = nil
	} else {
		d.tail = nil
	}
	d.size--
	return value, true
}

func (d *Deque[T]) PopBack() (T, bool) {
	var zero T
	if d.tail == nil {
		return zero, false
	}
	value := d.tail.value
	d.tail = d.tail.prev
	if d.tail != nil {
		d.tail.next = nil
	} else {
		d.head = nil
	}
	d.size--
	return value, true
}

func (d *Deque[T]) PeekFront() (T, bool) {
	var zero T
	if d.head == nil {
		return zero, false
	}
	return d.head.value, true
}

func (d *Deque[T]) PeekBack() (T, bool) {
	var zero T
	if d.tail == nil {
		return zero, false
	}
	return d.tail.value, true
}

func (d *Deque[T]) IsEmpty() bool {
	return d.size == 0
}

func (d *Deque[T]) Size() int {
	return d.size
}

func (d *Deque[T]) Clear() {
	d.head = nil
	d.tail = nil
	d.size = 0
}
