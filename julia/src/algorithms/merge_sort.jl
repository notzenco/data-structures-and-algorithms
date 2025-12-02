"""
Merge Sort - Divide and conquer stable sorting

Time Complexity: O(n log n)
Space Complexity: O(n)
Stable: Yes
"""

function _merge!(arr::AbstractVector{T}, left::Int, mid::Int, right::Int) where T
    left_size = mid - left + 1
    right_size = right - mid

    left_arr = arr[left:mid]
    right_arr = arr[(mid+1):right]

    i = 1
    j = 1
    k = left

    while i <= left_size && j <= right_size
        if left_arr[i] <= right_arr[j]
            arr[k] = left_arr[i]
            i += 1
        else
            arr[k] = right_arr[j]
            j += 1
        end
        k += 1
    end

    while i <= left_size
        arr[k] = left_arr[i]
        i += 1
        k += 1
    end

    while j <= right_size
        arr[k] = right_arr[j]
        j += 1
        k += 1
    end
end

function _merge_sort_range!(arr::AbstractVector{T}, left::Int, right::Int) where T
    if left < right
        mid = left + (right - left) ÷ 2
        _merge_sort_range!(arr, left, mid)
        _merge_sort_range!(arr, mid + 1, right)
        _merge!(arr, left, mid, right)
    end
end

"""
    merge_sort!(arr::AbstractVector{T}) where T

Sort array in-place using merge sort.
"""
function merge_sort!(arr::AbstractVector{T}) where T
    if length(arr) > 1
        _merge_sort_range!(arr, 1, length(arr))
    end
    arr
end

"""
    merge_sort_iterative!(arr::AbstractVector{T}) where T

Sort array using iterative (bottom-up) merge sort.
"""
function merge_sort_iterative!(arr::AbstractVector{T}) where T
    n = length(arr)
    size = 1
    while size < n
        left = 1
        while left <= n - size
            mid = left + size - 1
            right = min(left + 2 * size - 1, n)
            _merge!(arr, left, mid, right)
            left += 2 * size
        end
        size *= 2
    end
    arr
end

"""
    merge_sorted(arr::AbstractVector{T}) where T

Return a sorted copy using merge sort.
"""
function merge_sorted(arr::AbstractVector{T}) where T
    result = copy(arr)
    merge_sort!(result)
    result
end

"""
    merge_sort_by!(arr::AbstractVector{T}, key_func::Function) where T

Sort array by key function using merge sort.
"""
function merge_sort_by!(arr::AbstractVector{T}, key_func::Function) where T
    function merge_by!(left::Int, mid::Int, right::Int)
        left_size = mid - left + 1
        right_size = right - mid

        left_arr = arr[left:mid]
        right_arr = arr[(mid+1):right]

        i = 1
        j = 1
        k = left

        while i <= left_size && j <= right_size
            if key_func(left_arr[i]) <= key_func(right_arr[j])
                arr[k] = left_arr[i]
                i += 1
            else
                arr[k] = right_arr[j]
                j += 1
            end
            k += 1
        end

        while i <= left_size
            arr[k] = left_arr[i]
            i += 1
            k += 1
        end

        while j <= right_size
            arr[k] = right_arr[j]
            j += 1
            k += 1
        end
    end

    function sort_by!(left::Int, right::Int)
        if left < right
            mid = left + (right - left) ÷ 2
            sort_by!(left, mid)
            sort_by!(mid + 1, right)
            merge_by!(left, mid, right)
        end
    end

    if length(arr) > 1
        sort_by!(1, length(arr))
    end
    arr
end
