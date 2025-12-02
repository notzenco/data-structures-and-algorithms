"""Queue implementation using collections.deque.

Time: O(1) for enqueue, dequeue, peek
Space: O(n)
"""

from collections import deque
from typing import Generic, TypeVar, Optional

T = TypeVar("T")


class Queue(Generic[T]):
    """A generic queue implementation."""

    def __init__(self) -> None:
        self._data: deque[T] = deque()

    def enqueue(self, value: T) -> None:
        """Add a value to the back of the queue."""
        self._data.append(value)

    def dequeue(self) -> Optional[T]:
        """Remove and return the front value, or None if empty."""
        if self._data:
            return self._data.popleft()
        return None

    def peek(self) -> Optional[T]:
        """Return the front value without removing it, or None if empty."""
        if self._data:
            return self._data[0]
        return None

    def is_empty(self) -> bool:
        """Return True if the queue is empty."""
        return len(self._data) == 0

    def __len__(self) -> int:
        """Return the number of elements in the queue."""
        return len(self._data)

    def clear(self) -> None:
        """Remove all elements from the queue."""
        self._data.clear()
