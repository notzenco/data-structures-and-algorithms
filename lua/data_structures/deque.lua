-- Deque - Double-ended queue
--
-- Time Complexity:
-- - pushFront: O(1) amortized
-- - pushBack: O(1) amortized
-- - popFront: O(1) amortized
-- - popBack: O(1)

local Deque = {}
Deque.__index = Deque

function Deque.new()
    local self = setmetatable({}, Deque)
    self.items = {}
    self.front = 0
    self.back = -1
    return self
end

function Deque:pushFront(item)
    self.items[self.front - 1] = item
    self.front = self.front - 1
end

function Deque:pushBack(item)
    self.back = self.back + 1
    self.items[self.back] = item
end

function Deque:popFront()
    if self:isEmpty() then
        error("Deque is empty")
    end
    local item = self.items[self.front]
    self.items[self.front] = nil
    self.front = self.front + 1
    return item
end

function Deque:popBack()
    if self:isEmpty() then
        error("Deque is empty")
    end
    local item = self.items[self.back]
    self.items[self.back] = nil
    self.back = self.back - 1
    return item
end

function Deque:peekFront()
    if self:isEmpty() then
        error("Deque is empty")
    end
    return self.items[self.front]
end

function Deque:peekBack()
    if self:isEmpty() then
        error("Deque is empty")
    end
    return self.items[self.back]
end

function Deque:isEmpty()
    return self.front > self.back
end

function Deque:size()
    return self.back - self.front + 1
end

function Deque:clear()
    self.items = {}
    self.front = 0
    self.back = -1
end

function Deque:toArray()
    local result = {}
    local index = 1
    for i = self.front, self.back do
        result[index] = self.items[i]
        index = index + 1
    end
    return result
end

return Deque
