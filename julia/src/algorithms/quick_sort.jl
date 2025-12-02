"""
Quick Sort - Divide and conquer sorting with median-of-three pivot

Time Complexity: O(n log n) average, O(n²) worst case
Space Complexity: O(log n)
Stable: No
"""

function _median_of_three!(arr::AbstractVector{T}, left::Int, right::Int) where T
    mid = left + (right - left) ÷ 2

    # Sort left, mid, right
    if arr[left] > arr[mid]
        arr[left], arr[mid] = arr[mid], arr[left]
    end
    if arr[left] > arr[right]
        arr[left], arr[right] = arr[right], arr[left]
    end
    if arr[mid] > arr[right]
        arr[mid], arr[right] = arr[right], arr[mid]
    end

    # Move median to right-1
    arr[mid], arr[right-1] = arr[right-1], arr[mid]
    right - 1
end

function _partition!(arr::AbstractVector{T}, left::Int, right::Int) where T
    pivot_index = _median_of_three!(arr, left, right)
    pivot = arr[pivot_index]

    i = left
    j = pivot_index - 1

    while true
        while arr[i] < pivot
            i += 1
        end
        while j > left && arr[j] > pivot
            j -= 1
        end

        if i >= j
            break
        end

        arr[i], arr[j] = arr[j], arr[i]
        i += 1
        j -= 1
    end

    arr[i], arr[pivot_index] = arr[pivot_index], arr[i]
    i
end

function _partition_simple!(arr::AbstractVector{T}, left::Int, right::Int) where T
    pivot = arr[right]
    i = left - 1

    for j in left:(right-1)
        if arr[j] <= pivot
            i += 1
            arr[i], arr[j] = arr[j], arr[i]
        end
    end

    arr[i + 1], arr[right] = arr[right], arr[i + 1]
    i + 1
end

function _quick_sort_range!(arr::AbstractVector{T}, left::Int, right::Int) where T
    if right - left < 10
        # Use insertion sort for small arrays
        for i in (left + 1):right
            key = arr[i]
            j = i - 1
            while j >= left && arr[j] > key
                arr[j + 1] = arr[j]
                j -= 1
            end
            arr[j + 1] = key
        end
    elseif left < right
        pivot_index = _partition!(arr, left, right)
        _quick_sort_range!(arr, left, pivot_index - 1)
        _quick_sort_range!(arr, pivot_index + 1, right)
    end
end

"""
    quick_sort!(arr::AbstractVector{T}) where T

Sort array in-place using quick sort with median-of-three pivot.
"""
function quick_sort!(arr::AbstractVector{T}) where T
    if length(arr) > 1
        _quick_sort_range!(arr, 1, length(arr))
    end
    arr
end

function _quick_sort_simple_range!(arr::AbstractVector{T}, left::Int, right::Int) where T
    if left < right
        pivot_index = _partition_simple!(arr, left, right)
        _quick_sort_simple_range!(arr, left, pivot_index - 1)
        _quick_sort_simple_range!(arr, pivot_index + 1, right)
    end
end

"""
    quick_sort_simple!(arr::AbstractVector{T}) where T

Sort array using simple quick sort (rightmost pivot).
"""
function quick_sort_simple!(arr::AbstractVector{T}) where T
    if length(arr) > 1
        _quick_sort_simple_range!(arr, 1, length(arr))
    end
    arr
end

"""
    quick_sorted(arr::AbstractVector{T}) where T

Return a sorted copy using quick sort.
"""
function quick_sorted(arr::AbstractVector{T}) where T
    result = copy(arr)
    quick_sort!(result)
    result
end

function _quick_select!(arr::AbstractVector{T}, left::Int, right::Int, k::Int) where T
    if left == right
        return arr[left]
    end

    pivot_index = _partition_simple!(arr, left, right)

    if k == pivot_index
        return arr[k]
    elseif k < pivot_index
        return _quick_select!(arr, left, pivot_index - 1, k)
    else
        return _quick_select!(arr, pivot_index + 1, right, k)
    end
end

"""
    quick_select!(arr::AbstractVector{T}, k::Int) where T

Find the k-th smallest element (1-indexed).
Modifies the array.
"""
function quick_select!(arr::AbstractVector{T}, k::Int) where T
    if k < 1 || k > length(arr)
        throw(BoundsError(arr, k))
    end
    _quick_select!(arr, 1, length(arr), k)
end

"""
    find_median!(arr::AbstractVector{T}) where T

Find median of array (modifies array).
"""
function find_median!(arr::AbstractVector{T}) where T
    quick_select!(arr, (length(arr) + 1) ÷ 2)
end
