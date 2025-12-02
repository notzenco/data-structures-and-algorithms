package datastructures

// Stack implements a LIFO stack using a linked list.
// Time: O(1) for all operations
// Space: O(n)
type Stack[T any] struct {
	head *stackNode[T]
	size int
}

type stackNode[T any] struct {
	value T
	next  *stackNode[T]
}

func NewStack[T any]() *Stack[T] {
	return &Stack[T]{}
}

func (s *Stack[T]) Push(value T) {
	s.head = &stackNode[T]{value: value, next: s.head}
	s.size++
}

func (s *Stack[T]) Pop() (T, bool) {
	var zero T
	if s.head == nil {
		return zero, false
	}
	value := s.head.value
	s.head = s.head.next
	s.size--
	return value, true
}

func (s *Stack[T]) Peek() (T, bool) {
	var zero T
	if s.head == nil {
		return zero, false
	}
	return s.head.value, true
}

func (s *Stack[T]) IsEmpty() bool {
	return s.size == 0
}

func (s *Stack[T]) Size() int {
	return s.size
}

func (s *Stack[T]) Clear() {
	s.head = nil
	s.size = 0
}
