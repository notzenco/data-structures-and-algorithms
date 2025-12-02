"""Doubly linked list implementation.

Time: O(1) for push/pop front/back, O(n) for search
Space: O(n)
"""

from typing import Generic, TypeVar, Optional

T = TypeVar("T")


class _Node(Generic[T]):
    """Internal node class."""

    def __init__(self, value: T) -> None:
        self.value = value
        self.prev: Optional[_Node[T]] = None
        self.next: Optional[_Node[T]] = None


class DoublyLinkedList(Generic[T]):
    """A generic doubly linked list implementation."""

    def __init__(self) -> None:
        self._head: Optional[_Node[T]] = None
        self._tail: Optional[_Node[T]] = None
        self._len = 0

    def push_front(self, value: T) -> None:
        """Add a value to the front of the list."""
        node = _Node(value)
        if self._head is None:
            self._head = self._tail = node
        else:
            node.next = self._head
            self._head.prev = node
            self._head = node
        self._len += 1

    def push_back(self, value: T) -> None:
        """Add a value to the back of the list."""
        node = _Node(value)
        if self._tail is None:
            self._head = self._tail = node
        else:
            node.prev = self._tail
            self._tail.next = node
            self._tail = node
        self._len += 1

    def pop_front(self) -> Optional[T]:
        """Remove and return the front value, or None if empty."""
        if self._head is None:
            return None
        value = self._head.value
        self._head = self._head.next
        if self._head is None:
            self._tail = None
        else:
            self._head.prev = None
        self._len -= 1
        return value

    def pop_back(self) -> Optional[T]:
        """Remove and return the back value, or None if empty."""
        if self._tail is None:
            return None
        value = self._tail.value
        self._tail = self._tail.prev
        if self._tail is None:
            self._head = None
        else:
            self._tail.next = None
        self._len -= 1
        return value

    def peek_front(self) -> Optional[T]:
        """Return the front value without removing it, or None if empty."""
        if self._head is None:
            return None
        return self._head.value

    def peek_back(self) -> Optional[T]:
        """Return the back value without removing it, or None if empty."""
        if self._tail is None:
            return None
        return self._tail.value

    def is_empty(self) -> bool:
        """Return True if the list is empty."""
        return self._head is None

    def __len__(self) -> int:
        """Return the number of elements."""
        return self._len

    def clear(self) -> None:
        """Remove all elements."""
        self._head = None
        self._tail = None
        self._len = 0
