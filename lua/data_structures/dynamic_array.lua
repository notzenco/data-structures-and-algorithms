-- Dynamic Array - Resizable array implementation
--
-- Time Complexity:
-- - get/set: O(1)
-- - push: O(1) amortized
-- - pop: O(1)
-- - insert: O(n)
-- - removeAt: O(n)

local DynamicArray = {}
DynamicArray.__index = DynamicArray

function DynamicArray.new()
    local self = setmetatable({}, DynamicArray)
    self.items = {}
    self.count = 0
    return self
end

function DynamicArray:push(item)
    self.count = self.count + 1
    self.items[self.count] = item
end

function DynamicArray:pop()
    if self.count == 0 then
        error("Array is empty")
    end
    local item = self.items[self.count]
    self.items[self.count] = nil
    self.count = self.count - 1
    return item
end

function DynamicArray:get(index)
    if index < 1 or index > self.count then
        error("Index out of bounds")
    end
    return self.items[index]
end

function DynamicArray:set(index, item)
    if index < 1 or index > self.count then
        error("Index out of bounds")
    end
    self.items[index] = item
end

function DynamicArray:insert(index, item)
    if index < 1 or index > self.count + 1 then
        error("Index out of bounds")
    end
    for i = self.count, index, -1 do
        self.items[i + 1] = self.items[i]
    end
    self.items[index] = item
    self.count = self.count + 1
end

function DynamicArray:removeAt(index)
    if index < 1 or index > self.count then
        error("Index out of bounds")
    end
    local item = self.items[index]
    for i = index, self.count - 1 do
        self.items[i] = self.items[i + 1]
    end
    self.items[self.count] = nil
    self.count = self.count - 1
    return item
end

function DynamicArray:find(item)
    for i = 1, self.count do
        if self.items[i] == item then
            return i
        end
    end
    return nil
end

function DynamicArray:contains(item)
    return self:find(item) ~= nil
end

function DynamicArray:isEmpty()
    return self.count == 0
end

function DynamicArray:size()
    return self.count
end

function DynamicArray:clear()
    self.items = {}
    self.count = 0
end

function DynamicArray:toArray()
    local result = {}
    for i = 1, self.count do
        result[i] = self.items[i]
    end
    return result
end

return DynamicArray
