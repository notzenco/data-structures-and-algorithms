-- Stack - LIFO data structure
--
-- Time Complexity:
-- - push: O(1)
-- - pop: O(1)
-- - peek: O(1)
-- - isEmpty: O(1)
-- - size: O(1)

local Stack = {}
Stack.__index = Stack

function Stack.new()
    local self = setmetatable({}, Stack)
    self.items = {}
    self.count = 0
    return self
end

function Stack:push(item)
    self.count = self.count + 1
    self.items[self.count] = item
end

function Stack:pop()
    if self.count == 0 then
        error("Stack is empty")
    end
    local item = self.items[self.count]
    self.items[self.count] = nil
    self.count = self.count - 1
    return item
end

function Stack:peek()
    if self.count == 0 then
        error("Stack is empty")
    end
    return self.items[self.count]
end

function Stack:isEmpty()
    return self.count == 0
end

function Stack:size()
    return self.count
end

function Stack:clear()
    self.items = {}
    self.count = 0
end

function Stack:toArray()
    local result = {}
    for i = 1, self.count do
        result[i] = self.items[i]
    end
    return result
end

return Stack
