"""
Insertion Sort - Simple stable sorting algorithm

Time Complexity: O(n²)
Space Complexity: O(1)
Stable: Yes
"""

"""
    insertion_sort!(arr::AbstractVector{T}) where T

Sort array in-place using insertion sort.
"""
function insertion_sort!(arr::AbstractVector{T}) where T
    for i in 2:length(arr)
        key = arr[i]
        j = i - 1
        while j >= 1 && arr[j] > key
            arr[j + 1] = arr[j]
            j -= 1
        end
        arr[j + 1] = key
    end
    arr
end

"""
    insertion_sort_desc!(arr::AbstractVector{T}) where T

Sort array in-place in descending order.
"""
function insertion_sort_desc!(arr::AbstractVector{T}) where T
    for i in 2:length(arr)
        key = arr[i]
        j = i - 1
        while j >= 1 && arr[j] < key
            arr[j + 1] = arr[j]
            j -= 1
        end
        arr[j + 1] = key
    end
    arr
end

"""
    insertion_sort_by!(arr::AbstractVector{T}, key_func::Function) where T

Sort array in-place by key function.
"""
function insertion_sort_by!(arr::AbstractVector{T}, key_func::Function) where T
    for i in 2:length(arr)
        key = arr[i]
        key_val = key_func(key)
        j = i - 1
        while j >= 1 && key_func(arr[j]) > key_val
            arr[j + 1] = arr[j]
            j -= 1
        end
        arr[j + 1] = key
    end
    arr
end

"""
    insertion_sort_range!(arr::AbstractVector{T}, start::Int, stop::Int) where T

Sort a range of array in-place.
"""
function insertion_sort_range!(arr::AbstractVector{T}, start::Int, stop::Int) where T
    actual_stop = min(stop, length(arr))
    for i in (start + 1):actual_stop
        key = arr[i]
        j = i - 1
        while j >= start && arr[j] > key
            arr[j + 1] = arr[j]
            j -= 1
        end
        arr[j + 1] = key
    end
    arr
end

"""
    insertion_sorted(arr::AbstractVector{T}) where T

Return a sorted copy of the array.
"""
function insertion_sorted(arr::AbstractVector{T}) where T
    result = copy(arr)
    insertion_sort!(result)
    result
end

"""
    insertion_sorted_desc(arr::AbstractVector{T}) where T

Return a sorted copy in descending order.
"""
function insertion_sorted_desc(arr::AbstractVector{T}) where T
    result = copy(arr)
    insertion_sort_desc!(result)
    result
end

"""
    is_sorted(arr::AbstractVector{T}) where T

Check if array is sorted in ascending order.
"""
function is_sorted(arr::AbstractVector{T}) where T
    for i in 2:length(arr)
        if arr[i] < arr[i - 1]
            return false
        end
    end
    true
end

"""
    is_sorted_desc(arr::AbstractVector{T}) where T

Check if array is sorted in descending order.
"""
function is_sorted_desc(arr::AbstractVector{T}) where T
    for i in 2:length(arr)
        if arr[i] > arr[i - 1]
            return false
        end
    end
    true
end
