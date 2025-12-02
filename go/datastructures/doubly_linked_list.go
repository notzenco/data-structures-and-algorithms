package datastructures

// DoublyLinkedList implements a doubly linked list.
// Time: O(1) for front/back ops, O(n) for search
// Space: O(n)
type DoublyLinkedList[T comparable] struct {
	head *doublyNode[T]
	tail *doublyNode[T]
	size int
}

type doublyNode[T any] struct {
	value T
	prev  *doublyNode[T]
	next  *doublyNode[T]
}

func NewDoublyLinkedList[T comparable]() *DoublyLinkedList[T] {
	return &DoublyLinkedList[T]{}
}

func (l *DoublyLinkedList[T]) PushFront(value T) {
	node := &doublyNode[T]{value: value, next: l.head}
	if l.head != nil {
		l.head.prev = node
	} else {
		l.tail = node
	}
	l.head = node
	l.size++
}

func (l *DoublyLinkedList[T]) PushBack(value T) {
	node := &doublyNode[T]{value: value, prev: l.tail}
	if l.tail != nil {
		l.tail.next = node
	} else {
		l.head = node
	}
	l.tail = node
	l.size++
}

func (l *DoublyLinkedList[T]) PopFront() (T, bool) {
	var zero T
	if l.head == nil {
		return zero, false
	}
	value := l.head.value
	l.head = l.head.next
	if l.head != nil {
		l.head.prev = nil
	} else {
		l.tail = nil
	}
	l.size--
	return value, true
}

func (l *DoublyLinkedList[T]) PopBack() (T, bool) {
	var zero T
	if l.tail == nil {
		return zero, false
	}
	value := l.tail.value
	l.tail = l.tail.prev
	if l.tail != nil {
		l.tail.next = nil
	} else {
		l.head = nil
	}
	l.size--
	return value, true
}

func (l *DoublyLinkedList[T]) PeekFront() (T, bool) {
	var zero T
	if l.head == nil {
		return zero, false
	}
	return l.head.value, true
}

func (l *DoublyLinkedList[T]) PeekBack() (T, bool) {
	var zero T
	if l.tail == nil {
		return zero, false
	}
	return l.tail.value, true
}

func (l *DoublyLinkedList[T]) Contains(value T) bool {
	current := l.head
	for current != nil {
		if current.value == value {
			return true
		}
		current = current.next
	}
	return false
}

func (l *DoublyLinkedList[T]) Remove(value T) bool {
	current := l.head
	for current != nil {
		if current.value == value {
			if current.prev != nil {
				current.prev.next = current.next
			} else {
				l.head = current.next
			}
			if current.next != nil {
				current.next.prev = current.prev
			} else {
				l.tail = current.prev
			}
			l.size--
			return true
		}
		current = current.next
	}
	return false
}

func (l *DoublyLinkedList[T]) IsEmpty() bool {
	return l.size == 0
}

func (l *DoublyLinkedList[T]) Size() int {
	return l.size
}

func (l *DoublyLinkedList[T]) ToSlice() []T {
	result := make([]T, 0, l.size)
	current := l.head
	for current != nil {
		result = append(result, current.value)
		current = current.next
	}
	return result
}

func (l *DoublyLinkedList[T]) Clear() {
	l.head = nil
	l.tail = nil
	l.size = 0
}
