-- Quick Sort - Divide and conquer sorting with median-of-three pivot
--
-- Time Complexity: O(n log n) average, O(n²) worst case
-- Space Complexity: O(log n)
-- Stable: No

local quick_sort = {}

local function medianOfThree(arr, left, right)
    local mid = math.floor(left + (right - left) / 2)

    if arr[left] > arr[mid] then
        arr[left], arr[mid] = arr[mid], arr[left]
    end
    if arr[left] > arr[right] then
        arr[left], arr[right] = arr[right], arr[left]
    end
    if arr[mid] > arr[right] then
        arr[mid], arr[right] = arr[right], arr[mid]
    end

    arr[mid], arr[right - 1] = arr[right - 1], arr[mid]
    return right - 1
end

local function partition(arr, left, right)
    local pivotIndex = medianOfThree(arr, left, right)
    local pivot = arr[pivotIndex]

    local i = left
    local j = pivotIndex - 1

    while true do
        while arr[i] < pivot do
            i = i + 1
        end
        while j > left and arr[j] > pivot do
            j = j - 1
        end

        if i >= j then
            break
        end

        arr[i], arr[j] = arr[j], arr[i]
        i = i + 1
        j = j - 1
    end

    arr[i], arr[pivotIndex] = arr[pivotIndex], arr[i]
    return i
end

local function partitionSimple(arr, left, right)
    local pivot = arr[right]
    local i = left - 1

    for j = left, right - 1 do
        if arr[j] <= pivot then
            i = i + 1
            arr[i], arr[j] = arr[j], arr[i]
        end
    end

    arr[i + 1], arr[right] = arr[right], arr[i + 1]
    return i + 1
end

local function quickSortRange(arr, left, right)
    if right - left < 10 then
        -- Use insertion sort for small arrays
        for i = left + 1, right do
            local key = arr[i]
            local j = i - 1
            while j >= left and arr[j] > key do
                arr[j + 1] = arr[j]
                j = j - 1
            end
            arr[j + 1] = key
        end
    elseif left < right then
        local pivotIndex = partition(arr, left, right)
        quickSortRange(arr, left, pivotIndex - 1)
        quickSortRange(arr, pivotIndex + 1, right)
    end
end

function quick_sort.sort(arr)
    if #arr > 1 then
        quickSortRange(arr, 1, #arr)
    end
    return arr
end

local function quickSortSimpleRange(arr, left, right)
    if left < right then
        local pivotIndex = partitionSimple(arr, left, right)
        quickSortSimpleRange(arr, left, pivotIndex - 1)
        quickSortSimpleRange(arr, pivotIndex + 1, right)
    end
end

function quick_sort.sortSimple(arr)
    if #arr > 1 then
        quickSortSimpleRange(arr, 1, #arr)
    end
    return arr
end

function quick_sort.sorted(arr)
    local result = {}
    for i = 1, #arr do
        result[i] = arr[i]
    end
    return quick_sort.sort(result)
end

local function quickSelectRange(arr, left, right, k)
    if left == right then
        return arr[left]
    end

    local pivotIndex = partitionSimple(arr, left, right)

    if k == pivotIndex then
        return arr[k]
    elseif k < pivotIndex then
        return quickSelectRange(arr, left, pivotIndex - 1, k)
    else
        return quickSelectRange(arr, pivotIndex + 1, right, k)
    end
end

function quick_sort.quickSelect(arr, k)
    if k < 1 or k > #arr then
        error("k out of bounds")
    end
    return quickSelectRange(arr, 1, #arr, k)
end

function quick_sort.findMedian(arr)
    return quick_sort.quickSelect(arr, math.floor((#arr + 1) / 2))
end

return quick_sort
