package datastructures

// SinglyLinkedList implements a singly linked list.
// Time: O(1) front ops, O(n) back ops and search
// Space: O(n)
type SinglyLinkedList[T comparable] struct {
	head *singlyNode[T]
	tail *singlyNode[T]
	size int
}

type singlyNode[T any] struct {
	value T
	next  *singlyNode[T]
}

func NewSinglyLinkedList[T comparable]() *SinglyLinkedList[T] {
	return &SinglyLinkedList[T]{}
}

func (l *SinglyLinkedList[T]) PushFront(value T) {
	node := &singlyNode[T]{value: value, next: l.head}
	l.head = node
	if l.tail == nil {
		l.tail = node
	}
	l.size++
}

func (l *SinglyLinkedList[T]) PushBack(value T) {
	node := &singlyNode[T]{value: value}
	if l.tail != nil {
		l.tail.next = node
	} else {
		l.head = node
	}
	l.tail = node
	l.size++
}

func (l *SinglyLinkedList[T]) PopFront() (T, bool) {
	var zero T
	if l.head == nil {
		return zero, false
	}
	value := l.head.value
	l.head = l.head.next
	if l.head == nil {
		l.tail = nil
	}
	l.size--
	return value, true
}

func (l *SinglyLinkedList[T]) PeekFront() (T, bool) {
	var zero T
	if l.head == nil {
		return zero, false
	}
	return l.head.value, true
}

func (l *SinglyLinkedList[T]) PeekBack() (T, bool) {
	var zero T
	if l.tail == nil {
		return zero, false
	}
	return l.tail.value, true
}

func (l *SinglyLinkedList[T]) Contains(value T) bool {
	current := l.head
	for current != nil {
		if current.value == value {
			return true
		}
		current = current.next
	}
	return false
}

func (l *SinglyLinkedList[T]) Remove(value T) bool {
	if l.head == nil {
		return false
	}
	if l.head.value == value {
		l.head = l.head.next
		if l.head == nil {
			l.tail = nil
		}
		l.size--
		return true
	}
	current := l.head
	for current.next != nil {
		if current.next.value == value {
			if current.next == l.tail {
				l.tail = current
			}
			current.next = current.next.next
			l.size--
			return true
		}
		current = current.next
	}
	return false
}

func (l *SinglyLinkedList[T]) IsEmpty() bool {
	return l.size == 0
}

func (l *SinglyLinkedList[T]) Size() int {
	return l.size
}

func (l *SinglyLinkedList[T]) ToSlice() []T {
	result := make([]T, 0, l.size)
	current := l.head
	for current != nil {
		result = append(result, current.value)
		current = current.next
	}
	return result
}

func (l *SinglyLinkedList[T]) Clear() {
	l.head = nil
	l.tail = nil
	l.size = 0
}
