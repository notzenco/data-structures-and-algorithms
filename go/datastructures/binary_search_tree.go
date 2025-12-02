package datastructures

import "cmp"

// BinarySearchTree implements a binary search tree.
// Time: O(log n) average, O(n) worst for all operations
// Space: O(n)
type BinarySearchTree[T cmp.Ordered] struct {
	root *bstNode[T]
	size int
}

type bstNode[T any] struct {
	value T
	left  *bstNode[T]
	right *bstNode[T]
}

func NewBinarySearchTree[T cmp.Ordered]() *BinarySearchTree[T] {
	return &BinarySearchTree[T]{}
}

func (t *BinarySearchTree[T]) Insert(value T) {
	t.root = t.insertNode(t.root, value)
	t.size++
}

func (t *BinarySearchTree[T]) insertNode(node *bstNode[T], value T) *bstNode[T] {
	if node == nil {
		return &bstNode[T]{value: value}
	}

	if value < node.value {
		node.left = t.insertNode(node.left, value)
	} else if value > node.value {
		node.right = t.insertNode(node.right, value)
	}
	return node
}

func (t *BinarySearchTree[T]) Contains(value T) bool {
	return t.findNode(t.root, value) != nil
}

func (t *BinarySearchTree[T]) findNode(node *bstNode[T], value T) *bstNode[T] {
	if node == nil {
		return nil
	}

	if value < node.value {
		return t.findNode(node.left, value)
	} else if value > node.value {
		return t.findNode(node.right, value)
	}
	return node
}

func (t *BinarySearchTree[T]) Remove(value T) bool {
	sizeBefore := t.size
	t.root = t.removeNode(t.root, value)
	return t.size < sizeBefore
}

func (t *BinarySearchTree[T]) removeNode(node *bstNode[T], value T) *bstNode[T] {
	if node == nil {
		return nil
	}

	if value < node.value {
		node.left = t.removeNode(node.left, value)
	} else if value > node.value {
		node.right = t.removeNode(node.right, value)
	} else {
		t.size--

		if node.left == nil {
			return node.right
		}
		if node.right == nil {
			return node.left
		}

		minRight := t.findMinNode(node.right)
		node.value = minRight.value
		node.right = t.removeNode(node.right, minRight.value)
		t.size++ // Compensate for recursive remove
	}
	return node
}

func (t *BinarySearchTree[T]) findMinNode(node *bstNode[T]) *bstNode[T] {
	for node.left != nil {
		node = node.left
	}
	return node
}

func (t *BinarySearchTree[T]) findMaxNode(node *bstNode[T]) *bstNode[T] {
	for node.right != nil {
		node = node.right
	}
	return node
}

func (t *BinarySearchTree[T]) Min() (T, bool) {
	var zero T
	if t.root == nil {
		return zero, false
	}
	return t.findMinNode(t.root).value, true
}

func (t *BinarySearchTree[T]) Max() (T, bool) {
	var zero T
	if t.root == nil {
		return zero, false
	}
	return t.findMaxNode(t.root).value, true
}

func (t *BinarySearchTree[T]) InOrder() []T {
	result := make([]T, 0, t.size)
	t.inOrderTraverse(t.root, &result)
	return result
}

func (t *BinarySearchTree[T]) inOrderTraverse(node *bstNode[T], result *[]T) {
	if node == nil {
		return
	}
	t.inOrderTraverse(node.left, result)
	*result = append(*result, node.value)
	t.inOrderTraverse(node.right, result)
}

func (t *BinarySearchTree[T]) PreOrder() []T {
	result := make([]T, 0, t.size)
	t.preOrderTraverse(t.root, &result)
	return result
}

func (t *BinarySearchTree[T]) preOrderTraverse(node *bstNode[T], result *[]T) {
	if node == nil {
		return
	}
	*result = append(*result, node.value)
	t.preOrderTraverse(node.left, result)
	t.preOrderTraverse(node.right, result)
}

func (t *BinarySearchTree[T]) PostOrder() []T {
	result := make([]T, 0, t.size)
	t.postOrderTraverse(t.root, &result)
	return result
}

func (t *BinarySearchTree[T]) postOrderTraverse(node *bstNode[T], result *[]T) {
	if node == nil {
		return
	}
	t.postOrderTraverse(node.left, result)
	t.postOrderTraverse(node.right, result)
	*result = append(*result, node.value)
}

func (t *BinarySearchTree[T]) IsEmpty() bool {
	return t.size == 0
}

func (t *BinarySearchTree[T]) Size() int {
	return t.size
}

func (t *BinarySearchTree[T]) Clear() {
	t.root = nil
	t.size = 0
}
