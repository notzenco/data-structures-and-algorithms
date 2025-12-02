-- Queue - FIFO data structure
--
-- Time Complexity:
-- - enqueue: O(1)
-- - dequeue: O(1) amortized
-- - peek: O(1)
-- - isEmpty: O(1)
-- - size: O(1)

local Queue = {}
Queue.__index = Queue

function Queue.new()
    local self = setmetatable({}, Queue)
    self.items = {}
    self.front = 1
    self.back = 0
    return self
end

function Queue:enqueue(item)
    self.back = self.back + 1
    self.items[self.back] = item
end

function Queue:dequeue()
    if self:isEmpty() then
        error("Queue is empty")
    end
    local item = self.items[self.front]
    self.items[self.front] = nil
    self.front = self.front + 1

    -- Compact if too much wasted space
    if self.front > 16 and self.front > self:size() then
        local newItems = {}
        local newIndex = 1
        for i = self.front, self.back do
            newItems[newIndex] = self.items[i]
            newIndex = newIndex + 1
        end
        self.items = newItems
        self.back = self.back - self.front + 1
        self.front = 1
    end

    return item
end

function Queue:peek()
    if self:isEmpty() then
        error("Queue is empty")
    end
    return self.items[self.front]
end

function Queue:isEmpty()
    return self.front > self.back
end

function Queue:size()
    return self.back - self.front + 1
end

function Queue:clear()
    self.items = {}
    self.front = 1
    self.back = 0
end

function Queue:toArray()
    local result = {}
    local index = 1
    for i = self.front, self.back do
        result[index] = self.items[i]
        index = index + 1
    end
    return result
end

return Queue
