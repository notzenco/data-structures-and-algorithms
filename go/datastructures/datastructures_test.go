package datastructures

import (
	"testing"
)

func TestStack(t *testing.T) {
	stack := NewStack[int]()

	if !stack.IsEmpty() {
		t.Error("New stack should be empty")
	}

	stack.Push(1)
	stack.Push(2)
	stack.Push(3)

	if stack.Size() != 3 {
		t.Errorf("Expected size 3, got %d", stack.Size())
	}

	val, ok := stack.Peek()
	if !ok || val != 3 {
		t.Errorf("Expected peek 3, got %d", val)
	}

	val, ok = stack.Pop()
	if !ok || val != 3 {
		t.Errorf("Expected pop 3, got %d", val)
	}

	val, ok = stack.Pop()
	if !ok || val != 2 {
		t.Errorf("Expected pop 2, got %d", val)
	}
}

func TestQueue(t *testing.T) {
	queue := NewQueue[int]()

	if !queue.IsEmpty() {
		t.Error("New queue should be empty")
	}

	queue.Enqueue(1)
	queue.Enqueue(2)
	queue.Enqueue(3)

	if queue.Size() != 3 {
		t.Errorf("Expected size 3, got %d", queue.Size())
	}

	val, ok := queue.Peek()
	if !ok || val != 1 {
		t.Errorf("Expected peek 1, got %d", val)
	}

	val, ok = queue.Dequeue()
	if !ok || val != 1 {
		t.Errorf("Expected dequeue 1, got %d", val)
	}

	val, ok = queue.Dequeue()
	if !ok || val != 2 {
		t.Errorf("Expected dequeue 2, got %d", val)
	}
}

func TestDynamicArray(t *testing.T) {
	arr := NewDynamicArray[int](2)

	if !arr.IsEmpty() {
		t.Error("New array should be empty")
	}

	for i := 0; i < 100; i++ {
		arr.Push(i)
	}

	if arr.Size() != 100 {
		t.Errorf("Expected size 100, got %d", arr.Size())
	}

	val, ok := arr.Get(99)
	if !ok || val != 99 {
		t.Errorf("Expected get(99) = 99, got %d", val)
	}
}

func TestSinglyLinkedList(t *testing.T) {
	list := NewSinglyLinkedList[int]()

	list.PushFront(1)
	list.PushFront(2)
	list.PushBack(3)

	if list.Size() != 3 {
		t.Errorf("Expected size 3, got %d", list.Size())
	}

	front, _ := list.PeekFront()
	back, _ := list.PeekBack()

	if front != 2 {
		t.Errorf("Expected front 2, got %d", front)
	}
	if back != 3 {
		t.Errorf("Expected back 3, got %d", back)
	}

	if !list.Contains(1) {
		t.Error("List should contain 1")
	}
}

func TestDoublyLinkedList(t *testing.T) {
	list := NewDoublyLinkedList[int]()

	list.PushFront(1)
	list.PushFront(2)
	list.PushBack(3)

	if list.Size() != 3 {
		t.Errorf("Expected size 3, got %d", list.Size())
	}

	val, _ := list.PopFront()
	if val != 2 {
		t.Errorf("Expected pop front 2, got %d", val)
	}

	val, _ = list.PopBack()
	if val != 3 {
		t.Errorf("Expected pop back 3, got %d", val)
	}
}

func TestDeque(t *testing.T) {
	deque := NewDeque[int]()

	deque.PushFront(1)
	deque.PushBack(2)
	deque.PushFront(0)

	if deque.Size() != 3 {
		t.Errorf("Expected size 3, got %d", deque.Size())
	}

	front, _ := deque.PeekFront()
	back, _ := deque.PeekBack()

	if front != 0 {
		t.Errorf("Expected front 0, got %d", front)
	}
	if back != 2 {
		t.Errorf("Expected back 2, got %d", back)
	}
}

func TestBinarySearchTree(t *testing.T) {
	bst := NewBinarySearchTree[int]()

	bst.Insert(5)
	bst.Insert(3)
	bst.Insert(7)
	bst.Insert(1)
	bst.Insert(9)

	if bst.Size() != 5 {
		t.Errorf("Expected size 5, got %d", bst.Size())
	}

	if !bst.Contains(3) {
		t.Error("BST should contain 3")
	}
	if bst.Contains(4) {
		t.Error("BST should not contain 4")
	}

	inorder := bst.InOrder()
	expected := []int{1, 3, 5, 7, 9}
	for i, v := range inorder {
		if v != expected[i] {
			t.Errorf("Inorder mismatch at %d: expected %d, got %d", i, expected[i], v)
		}
	}
}

func TestMinHeap(t *testing.T) {
	heap := NewMinHeap[int]()

	heap.Insert(5)
	heap.Insert(3)
	heap.Insert(7)
	heap.Insert(1)

	if heap.Size() != 4 {
		t.Errorf("Expected size 4, got %d", heap.Size())
	}

	min, _ := heap.Peek()
	if min != 1 {
		t.Errorf("Expected peek 1, got %d", min)
	}

	min, _ = heap.ExtractMin()
	if min != 1 {
		t.Errorf("Expected extract 1, got %d", min)
	}

	min, _ = heap.Peek()
	if min != 3 {
		t.Errorf("Expected peek 3, got %d", min)
	}
}

func TestDisjointSet(t *testing.T) {
	ds := NewDisjointSet(5)

	if ds.Connected(0, 1) {
		t.Error("0 and 1 should not be connected initially")
	}

	ds.Union(0, 1)
	if !ds.Connected(0, 1) {
		t.Error("0 and 1 should be connected after union")
	}

	if ds.Count() != 4 {
		t.Errorf("Expected count 4, got %d", ds.Count())
	}

	ds.Union(2, 3)
	ds.Union(0, 2)

	if !ds.Connected(0, 3) {
		t.Error("0 and 3 should be connected")
	}

	if ds.Count() != 2 {
		t.Errorf("Expected count 2, got %d", ds.Count())
	}
}
