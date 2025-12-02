package datastructures

// DynamicArray implements a growable array with amortized O(1) append.
// Time: O(1) amortized push, O(n) insert/remove
// Space: O(n)
type DynamicArray[T any] struct {
	data     []T
	size     int
	capacity int
}

func NewDynamicArray[T any](initialCapacity ...int) *DynamicArray[T] {
	cap := 8
	if len(initialCapacity) > 0 && initialCapacity[0] > 0 {
		cap = initialCapacity[0]
	}
	return &DynamicArray[T]{
		data:     make([]T, cap),
		capacity: cap,
	}
}

func (a *DynamicArray[T]) Push(value T) {
	if a.size == a.capacity {
		a.resize(a.capacity * 2)
	}
	a.data[a.size] = value
	a.size++
}

func (a *DynamicArray[T]) Pop() (T, bool) {
	var zero T
	if a.size == 0 {
		return zero, false
	}
	a.size--
	value := a.data[a.size]
	a.data[a.size] = zero
	if a.size > 0 && a.size == a.capacity/4 {
		a.resize(a.capacity / 2)
	}
	return value, true
}

func (a *DynamicArray[T]) Get(index int) (T, bool) {
	var zero T
	if index < 0 || index >= a.size {
		return zero, false
	}
	return a.data[index], true
}

func (a *DynamicArray[T]) Set(index int, value T) bool {
	if index < 0 || index >= a.size {
		return false
	}
	a.data[index] = value
	return true
}

func (a *DynamicArray[T]) Insert(index int, value T) bool {
	if index < 0 || index > a.size {
		return false
	}
	if a.size == a.capacity {
		a.resize(a.capacity * 2)
	}
	for i := a.size; i > index; i-- {
		a.data[i] = a.data[i-1]
	}
	a.data[index] = value
	a.size++
	return true
}

func (a *DynamicArray[T]) Remove(index int) (T, bool) {
	var zero T
	if index < 0 || index >= a.size {
		return zero, false
	}
	value := a.data[index]
	for i := index; i < a.size-1; i++ {
		a.data[i] = a.data[i+1]
	}
	a.size--
	a.data[a.size] = zero
	if a.size > 0 && a.size == a.capacity/4 {
		a.resize(a.capacity / 2)
	}
	return value, true
}

func (a *DynamicArray[T]) IsEmpty() bool {
	return a.size == 0
}

func (a *DynamicArray[T]) Size() int {
	return a.size
}

func (a *DynamicArray[T]) Capacity() int {
	return a.capacity
}

func (a *DynamicArray[T]) ToSlice() []T {
	result := make([]T, a.size)
	copy(result, a.data[:a.size])
	return result
}

func (a *DynamicArray[T]) Clear() {
	var zero T
	for i := 0; i < a.size; i++ {
		a.data[i] = zero
	}
	a.size = 0
}

func (a *DynamicArray[T]) resize(newCapacity int) {
	newData := make([]T, newCapacity)
	copy(newData, a.data[:a.size])
	a.data = newData
	a.capacity = newCapacity
}
