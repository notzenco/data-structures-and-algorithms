"""Binary search tree implementation.

Time: O(log n) average, O(n) worst for insert, search, delete
Space: O(n)
"""

from typing import Generic, TypeVar, Optional, Callable

T = TypeVar("T")


class _Node(Generic[T]):
    """Internal node class."""

    def __init__(self, value: T) -> None:
        self.value = value
        self.left: Optional[_Node[T]] = None
        self.right: Optional[_Node[T]] = None


class BinarySearchTree(Generic[T]):
    """A generic binary search tree implementation."""

    def __init__(self, key: Optional[Callable[[T], any]] = None) -> None:
        self._root: Optional[_Node[T]] = None
        self._len = 0
        self._key = key or (lambda x: x)

    def insert(self, value: T) -> bool:
        """Insert a value. Returns True if inserted, False if duplicate."""
        if self._root is None:
            self._root = _Node(value)
            self._len += 1
            return True
        return self._insert(self._root, value)

    def _insert(self, node: _Node[T], value: T) -> bool:
        key_val = self._key(value)
        node_key = self._key(node.value)

        if key_val < node_key:
            if node.left is None:
                node.left = _Node(value)
                self._len += 1
                return True
            return self._insert(node.left, value)
        elif key_val > node_key:
            if node.right is None:
                node.right = _Node(value)
                self._len += 1
                return True
            return self._insert(node.right, value)
        return False  # Duplicate

    def contains(self, value: T) -> bool:
        """Return True if the value is in the tree."""
        return self._search(self._root, value) is not None

    def _search(self, node: Optional[_Node[T]], value: T) -> Optional[_Node[T]]:
        if node is None:
            return None
        key_val = self._key(value)
        node_key = self._key(node.value)

        if key_val < node_key:
            return self._search(node.left, value)
        elif key_val > node_key:
            return self._search(node.right, value)
        return node

    def remove(self, value: T) -> bool:
        """Remove a value. Returns True if removed, False if not found."""
        self._root, removed = self._remove(self._root, value)
        if removed:
            self._len -= 1
        return removed

    def _remove(
        self, node: Optional[_Node[T]], value: T
    ) -> tuple[Optional[_Node[T]], bool]:
        if node is None:
            return None, False

        key_val = self._key(value)
        node_key = self._key(node.value)

        if key_val < node_key:
            node.left, removed = self._remove(node.left, value)
            return node, removed
        elif key_val > node_key:
            node.right, removed = self._remove(node.right, value)
            return node, removed

        # Found the node to remove
        if node.left is None:
            return node.right, True
        if node.right is None:
            return node.left, True

        # Two children: find minimum in right subtree
        min_node = self._find_min(node.right)
        node.value = min_node.value
        node.right, _ = self._remove(node.right, min_node.value)
        return node, True

    def _find_min(self, node: _Node[T]) -> _Node[T]:
        while node.left is not None:
            node = node.left
        return node

    def min(self) -> Optional[T]:
        """Return the minimum value, or None if empty."""
        if self._root is None:
            return None
        return self._find_min(self._root).value

    def max(self) -> Optional[T]:
        """Return the maximum value, or None if empty."""
        if self._root is None:
            return None
        node = self._root
        while node.right is not None:
            node = node.right
        return node.value

    def inorder(self) -> list[T]:
        """Return values in sorted order."""
        result: list[T] = []
        self._inorder(self._root, result)
        return result

    def _inorder(self, node: Optional[_Node[T]], result: list[T]) -> None:
        if node is not None:
            self._inorder(node.left, result)
            result.append(node.value)
            self._inorder(node.right, result)

    def is_empty(self) -> bool:
        """Return True if the tree is empty."""
        return self._root is None

    def __len__(self) -> int:
        """Return the number of elements."""
        return self._len

    def __contains__(self, value: T) -> bool:
        """Check if value exists."""
        return self.contains(value)
