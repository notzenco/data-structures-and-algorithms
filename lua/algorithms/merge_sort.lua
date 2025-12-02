-- Merge Sort - Divide and conquer stable sorting
--
-- Time Complexity: O(n log n)
-- Space Complexity: O(n)
-- Stable: Yes

local merge_sort = {}

local function merge(arr, left, mid, right)
    local leftSize = mid - left + 1
    local rightSize = right - mid

    local leftArr = {}
    local rightArr = {}

    for i = 1, leftSize do
        leftArr[i] = arr[left + i - 1]
    end
    for i = 1, rightSize do
        rightArr[i] = arr[mid + i]
    end

    local i = 1
    local j = 1
    local k = left

    while i <= leftSize and j <= rightSize do
        if leftArr[i] <= rightArr[j] then
            arr[k] = leftArr[i]
            i = i + 1
        else
            arr[k] = rightArr[j]
            j = j + 1
        end
        k = k + 1
    end

    while i <= leftSize do
        arr[k] = leftArr[i]
        i = i + 1
        k = k + 1
    end

    while j <= rightSize do
        arr[k] = rightArr[j]
        j = j + 1
        k = k + 1
    end
end

local function mergeSortRange(arr, left, right)
    if left < right then
        local mid = math.floor(left + (right - left) / 2)
        mergeSortRange(arr, left, mid)
        mergeSortRange(arr, mid + 1, right)
        merge(arr, left, mid, right)
    end
end

function merge_sort.sort(arr)
    if #arr > 1 then
        mergeSortRange(arr, 1, #arr)
    end
    return arr
end

function merge_sort.sortIterative(arr)
    local n = #arr
    local size = 1
    while size < n do
        local left = 1
        while left <= n - size do
            local mid = left + size - 1
            local right = math.min(left + 2 * size - 1, n)
            merge(arr, left, mid, right)
            left = left + 2 * size
        end
        size = size * 2
    end
    return arr
end

function merge_sort.sorted(arr)
    local result = {}
    for i = 1, #arr do
        result[i] = arr[i]
    end
    return merge_sort.sort(result)
end

return merge_sort
