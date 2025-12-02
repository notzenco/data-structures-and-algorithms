"""Binary search implementations.

Time: O(log n)
Space: O(1) iterative, O(log n) recursive
"""

from typing import TypeVar, Sequence, Optional, Callable

T = TypeVar("T")


def binary_search(
    arr: Sequence[T], target: T, key: Optional[Callable[[T], any]] = None
) -> Optional[int]:
    """Binary search for target in a sorted sequence. Returns index or None."""
    if not arr:
        return None

    key_fn = key or (lambda x: x)
    target_key = key_fn(target)
    left, right = 0, len(arr) - 1

    while left <= right:
        mid = left + (right - left) // 2
        mid_key = key_fn(arr[mid])

        if mid_key == target_key:
            return mid
        elif mid_key < target_key:
            left = mid + 1
        else:
            right = mid - 1

    return None


def binary_search_recursive(
    arr: Sequence[T], target: T, key: Optional[Callable[[T], any]] = None
) -> Optional[int]:
    """Recursive binary search for target in a sorted sequence."""
    if not arr:
        return None

    key_fn = key or (lambda x: x)

    def search(left: int, right: int) -> Optional[int]:
        if left > right:
            return None

        mid = left + (right - left) // 2
        mid_key = key_fn(arr[mid])
        target_key = key_fn(target)

        if mid_key == target_key:
            return mid
        elif mid_key < target_key:
            return search(mid + 1, right)
        else:
            return search(left, mid - 1)

    return search(0, len(arr) - 1)


def lower_bound(
    arr: Sequence[T], target: T, key: Optional[Callable[[T], any]] = None
) -> int:
    """Find the first position where target could be inserted (first element >= target)."""
    key_fn = key or (lambda x: x)
    target_key = key_fn(target)
    left, right = 0, len(arr)

    while left < right:
        mid = left + (right - left) // 2
        if key_fn(arr[mid]) < target_key:
            left = mid + 1
        else:
            right = mid

    return left


def upper_bound(
    arr: Sequence[T], target: T, key: Optional[Callable[[T], any]] = None
) -> int:
    """Find the first position after target (first element > target)."""
    key_fn = key or (lambda x: x)
    target_key = key_fn(target)
    left, right = 0, len(arr)

    while left < right:
        mid = left + (right - left) // 2
        if key_fn(arr[mid]) <= target_key:
            left = mid + 1
        else:
            right = mid

    return left
