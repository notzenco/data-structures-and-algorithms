-- Binary Search - Search in sorted arrays
--
-- Time Complexity: O(log n)
-- Space Complexity: O(1)

local binary_search = {}

function binary_search.search(arr, target)
    local left = 1
    local right = #arr

    while left <= right do
        local mid = math.floor(left + (right - left) / 2)
        if arr[mid] == target then
            return mid
        elseif arr[mid] < target then
            left = mid + 1
        else
            right = mid - 1
        end
    end

    return nil
end

function binary_search.lowerBound(arr, target)
    local left = 1
    local right = #arr + 1

    while left < right do
        local mid = math.floor(left + (right - left) / 2)
        if arr[mid] < target then
            left = mid + 1
        else
            right = mid
        end
    end

    return left
end

function binary_search.upperBound(arr, target)
    local left = 1
    local right = #arr + 1

    while left < right do
        local mid = math.floor(left + (right - left) / 2)
        if arr[mid] <= target then
            left = mid + 1
        else
            right = mid
        end
    end

    return left
end

function binary_search.equalRange(arr, target)
    return binary_search.lowerBound(arr, target), binary_search.upperBound(arr, target)
end

function binary_search.countEqual(arr, target)
    local lower, upper = binary_search.equalRange(arr, target)
    return upper - lower
end

function binary_search.findFirst(arr, target)
    local lower = binary_search.lowerBound(arr, target)
    if lower <= #arr and arr[lower] == target then
        return lower
    end
    return nil
end

function binary_search.findLast(arr, target)
    local upper = binary_search.upperBound(arr, target)
    if upper > 1 and arr[upper - 1] == target then
        return upper - 1
    end
    return nil
end

function binary_search.contains(arr, target)
    return binary_search.search(arr, target) ~= nil
end

return binary_search
