-- Insertion Sort - Simple stable sorting algorithm
--
-- Time Complexity: O(n²)
-- Space Complexity: O(1)
-- Stable: Yes

local insertion_sort = {}

function insertion_sort.sort(arr)
    for i = 2, #arr do
        local key = arr[i]
        local j = i - 1
        while j >= 1 and arr[j] > key do
            arr[j + 1] = arr[j]
            j = j - 1
        end
        arr[j + 1] = key
    end
    return arr
end

function insertion_sort.sortDesc(arr)
    for i = 2, #arr do
        local key = arr[i]
        local j = i - 1
        while j >= 1 and arr[j] < key do
            arr[j + 1] = arr[j]
            j = j - 1
        end
        arr[j + 1] = key
    end
    return arr
end

function insertion_sort.sortBy(arr, keyFunc)
    for i = 2, #arr do
        local key = arr[i]
        local keyVal = keyFunc(key)
        local j = i - 1
        while j >= 1 and keyFunc(arr[j]) > keyVal do
            arr[j + 1] = arr[j]
            j = j - 1
        end
        arr[j + 1] = key
    end
    return arr
end

function insertion_sort.sortRange(arr, start, stop)
    local actualStop = math.min(stop, #arr)
    for i = start + 1, actualStop do
        local key = arr[i]
        local j = i - 1
        while j >= start and arr[j] > key do
            arr[j + 1] = arr[j]
            j = j - 1
        end
        arr[j + 1] = key
    end
    return arr
end

function insertion_sort.sorted(arr)
    local result = {}
    for i = 1, #arr do
        result[i] = arr[i]
    end
    return insertion_sort.sort(result)
end

function insertion_sort.isSorted(arr)
    for i = 2, #arr do
        if arr[i] < arr[i - 1] then
            return false
        end
    end
    return true
end

function insertion_sort.isSortedDesc(arr)
    for i = 2, #arr do
        if arr[i] > arr[i - 1] then
            return false
        end
    end
    return true
end

return insertion_sort
