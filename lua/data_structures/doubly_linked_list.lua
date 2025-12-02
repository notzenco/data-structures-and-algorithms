-- Doubly Linked List - Bidirectional linked list
--
-- Time Complexity:
-- - prepend: O(1)
-- - append: O(1)
-- - removeFirst: O(1)
-- - removeLast: O(1)
-- - get: O(n)

local DoublyLinkedList = {}
DoublyLinkedList.__index = DoublyLinkedList

local function newNode(data)
    return { data = data, prev = nil, next = nil }
end

function DoublyLinkedList.new()
    local self = setmetatable({}, DoublyLinkedList)
    self.head = nil
    self.tail = nil
    self.count = 0
    return self
end

function DoublyLinkedList:prepend(item)
    local node = newNode(item)
    node.next = self.head
    if self.head ~= nil then
        self.head.prev = node
    end
    self.head = node
    if self.tail == nil then
        self.tail = node
    end
    self.count = self.count + 1
end

function DoublyLinkedList:append(item)
    local node = newNode(item)
    node.prev = self.tail
    if self.tail ~= nil then
        self.tail.next = node
    end
    self.tail = node
    if self.head == nil then
        self.head = node
    end
    self.count = self.count + 1
end

function DoublyLinkedList:get(index)
    if index < 1 or index > self.count then
        error("Index out of bounds")
    end
    local current
    if index <= self.count / 2 then
        current = self.head
        for _ = 1, index - 1 do
            current = current.next
        end
    else
        current = self.tail
        for _ = 1, self.count - index do
            current = current.prev
        end
    end
    return current.data
end

function DoublyLinkedList:set(index, item)
    if index < 1 or index > self.count then
        error("Index out of bounds")
    end
    local current
    if index <= self.count / 2 then
        current = self.head
        for _ = 1, index - 1 do
            current = current.next
        end
    else
        current = self.tail
        for _ = 1, self.count - index do
            current = current.prev
        end
    end
    current.data = item
end

function DoublyLinkedList:removeFirst()
    if self.head == nil then
        error("List is empty")
    end
    local item = self.head.data
    self.head = self.head.next
    if self.head ~= nil then
        self.head.prev = nil
    else
        self.tail = nil
    end
    self.count = self.count - 1
    return item
end

function DoublyLinkedList:removeLast()
    if self.tail == nil then
        error("List is empty")
    end
    local item = self.tail.data
    self.tail = self.tail.prev
    if self.tail ~= nil then
        self.tail.next = nil
    else
        self.head = nil
    end
    self.count = self.count - 1
    return item
end

function DoublyLinkedList:isEmpty()
    return self.count == 0
end

function DoublyLinkedList:size()
    return self.count
end

function DoublyLinkedList:clear()
    self.head = nil
    self.tail = nil
    self.count = 0
end

function DoublyLinkedList:toArray()
    local result = {}
    local current = self.head
    local index = 1
    while current ~= nil do
        result[index] = current.data
        current = current.next
        index = index + 1
    end
    return result
end

return DoublyLinkedList
