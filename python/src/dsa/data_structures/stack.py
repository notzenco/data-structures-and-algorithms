"""Stack implementation using a list.

Time: O(1) for push, pop, peek
Space: O(n)
"""

from typing import Generic, TypeVar, Optional

T = TypeVar("T")


class Stack(Generic[T]):
    """A generic stack implementation."""

    def __init__(self) -> None:
        self._data: list[T] = []

    def push(self, value: T) -> None:
        """Push a value onto the stack."""
        self._data.append(value)

    def pop(self) -> Optional[T]:
        """Pop and return the top value, or None if empty."""
        if self._data:
            return self._data.pop()
        return None

    def peek(self) -> Optional[T]:
        """Return the top value without removing it, or None if empty."""
        if self._data:
            return self._data[-1]
        return None

    def is_empty(self) -> bool:
        """Return True if the stack is empty."""
        return len(self._data) == 0

    def __len__(self) -> int:
        """Return the number of elements in the stack."""
        return len(self._data)

    def clear(self) -> None:
        """Remove all elements from the stack."""
        self._data.clear()
