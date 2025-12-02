"""Deque implementation using collections.deque.

Time: O(1) for push/pop front/back
Space: O(n)
"""

from collections import deque
from typing import Generic, TypeVar, Optional

T = TypeVar("T")


class Deque(Generic[T]):
    """A generic deque implementation."""

    def __init__(self) -> None:
        self._data: deque[T] = deque()

    def push_front(self, value: T) -> None:
        """Add a value to the front."""
        self._data.appendleft(value)

    def push_back(self, value: T) -> None:
        """Add a value to the back."""
        self._data.append(value)

    def pop_front(self) -> Optional[T]:
        """Remove and return the front value, or None if empty."""
        if self._data:
            return self._data.popleft()
        return None

    def pop_back(self) -> Optional[T]:
        """Remove and return the back value, or None if empty."""
        if self._data:
            return self._data.pop()
        return None

    def peek_front(self) -> Optional[T]:
        """Return the front value without removing it, or None if empty."""
        if self._data:
            return self._data[0]
        return None

    def peek_back(self) -> Optional[T]:
        """Return the back value without removing it, or None if empty."""
        if self._data:
            return self._data[-1]
        return None

    def get(self, index: int) -> Optional[T]:
        """Return the value at the given index, or None if invalid."""
        if 0 <= index < len(self._data):
            return self._data[index]
        return None

    def is_empty(self) -> bool:
        """Return True if the deque is empty."""
        return len(self._data) == 0

    def __len__(self) -> int:
        """Return the number of elements."""
        return len(self._data)

    def clear(self) -> None:
        """Remove all elements."""
        self._data.clear()
