"""
Binary Search - Search in sorted arrays

Time Complexity: O(log n)
Space Complexity: O(1)
"""

"""
    binary_search(arr::AbstractVector{T}, target::T) where T

Search for target in sorted array.
Returns index of target or nothing if not found.
Uses 1-based indexing.
"""
function binary_search(arr::AbstractVector{T}, target::T) where T
    left = 1
    right = length(arr)

    while left <= right
        mid = left + (right - left) ÷ 2
        if arr[mid] == target
            return mid
        elseif arr[mid] < target
            left = mid + 1
        else
            right = mid - 1
        end
    end

    nothing
end

"""
    binary_search_recursive(arr::AbstractVector{T}, target::T) where T

Recursive binary search.
"""
function binary_search_recursive(arr::AbstractVector{T}, target::T) where T
    function search(left::Int, right::Int)
        if left > right
            return nothing
        end
        mid = left + (right - left) ÷ 2
        if arr[mid] == target
            return mid
        elseif arr[mid] < target
            return search(mid + 1, right)
        else
            return search(left, mid - 1)
        end
    end

    search(1, length(arr))
end

"""
    lower_bound(arr::AbstractVector{T}, target::T) where T

Find the first position where target could be inserted.
Returns the index of the first element >= target.
"""
function lower_bound(arr::AbstractVector{T}, target::T) where T
    left = 1
    right = length(arr) + 1

    while left < right
        mid = left + (right - left) ÷ 2
        if arr[mid] < target
            left = mid + 1
        else
            right = mid
        end
    end

    left
end

"""
    upper_bound(arr::AbstractVector{T}, target::T) where T

Find the last position where target could be inserted.
Returns the index of the first element > target.
"""
function upper_bound(arr::AbstractVector{T}, target::T) where T
    left = 1
    right = length(arr) + 1

    while left < right
        mid = left + (right - left) ÷ 2
        if arr[mid] <= target
            left = mid + 1
        else
            right = mid
        end
    end

    left
end

"""
    equal_range(arr::AbstractVector{T}, target::T) where T

Find the range of elements equal to target.
Returns (lower_bound, upper_bound).
"""
function equal_range(arr::AbstractVector{T}, target::T) where T
    (lower_bound(arr, target), upper_bound(arr, target))
end

"""
    count_equal(arr::AbstractVector{T}, target::T) where T

Count occurrences of target in sorted array.
"""
function count_equal(arr::AbstractVector{T}, target::T) where T
    lower, upper = equal_range(arr, target)
    upper - lower
end

"""
    find_first(arr::AbstractVector{T}, target::T) where T

Find first occurrence of target.
Returns nothing if not found.
"""
function find_first(arr::AbstractVector{T}, target::T) where T
    lower = lower_bound(arr, target)
    if lower <= length(arr) && arr[lower] == target
        return lower
    end
    nothing
end

"""
    find_last(arr::AbstractVector{T}, target::T) where T

Find last occurrence of target.
Returns nothing if not found.
"""
function find_last(arr::AbstractVector{T}, target::T) where T
    upper = upper_bound(arr, target)
    if upper > 1 && arr[upper - 1] == target
        return upper - 1
    end
    nothing
end
