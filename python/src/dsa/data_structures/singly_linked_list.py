"""Singly linked list implementation.

Time: O(1) for push_front, O(n) for push_back, search
Space: O(n)
"""

from typing import Generic, TypeVar, Optional

T = TypeVar("T")


class _Node(Generic[T]):
    """Internal node class."""

    def __init__(self, value: T) -> None:
        self.value = value
        self.next: Optional[_Node[T]] = None


class SinglyLinkedList(Generic[T]):
    """A generic singly linked list implementation."""

    def __init__(self) -> None:
        self._head: Optional[_Node[T]] = None
        self._len = 0

    def push_front(self, value: T) -> None:
        """Add a value to the front of the list."""
        node = _Node(value)
        node.next = self._head
        self._head = node
        self._len += 1

    def pop_front(self) -> Optional[T]:
        """Remove and return the front value, or None if empty."""
        if self._head is None:
            return None
        value = self._head.value
        self._head = self._head.next
        self._len -= 1
        return value

    def push_back(self, value: T) -> None:
        """Add a value to the back of the list."""
        node = _Node(value)
        if self._head is None:
            self._head = node
        else:
            current = self._head
            while current.next is not None:
                current = current.next
            current.next = node
        self._len += 1

    def peek_front(self) -> Optional[T]:
        """Return the front value without removing it, or None if empty."""
        if self._head is None:
            return None
        return self._head.value

    def contains(self, value: T) -> bool:
        """Return True if the value is in the list."""
        current = self._head
        while current is not None:
            if current.value == value:
                return True
            current = current.next
        return False

    def is_empty(self) -> bool:
        """Return True if the list is empty."""
        return self._head is None

    def __len__(self) -> int:
        """Return the number of elements."""
        return self._len

    def clear(self) -> None:
        """Remove all elements."""
        self._head = None
        self._len = 0
