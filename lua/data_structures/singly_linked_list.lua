-- Singly Linked List - Forward-only linked list
--
-- Time Complexity:
-- - prepend: O(1)
-- - append: O(1) with tail pointer
-- - get: O(n)
-- - removeAt: O(n)

local SinglyLinkedList = {}
SinglyLinkedList.__index = SinglyLinkedList

local function newNode(data)
    return { data = data, next = nil }
end

function SinglyLinkedList.new()
    local self = setmetatable({}, SinglyLinkedList)
    self.head = nil
    self.tail = nil
    self.count = 0
    return self
end

function SinglyLinkedList:prepend(item)
    local node = newNode(item)
    node.next = self.head
    self.head = node
    if self.tail == nil then
        self.tail = node
    end
    self.count = self.count + 1
end

function SinglyLinkedList:append(item)
    local node = newNode(item)
    if self.tail == nil then
        self.head = node
        self.tail = node
    else
        self.tail.next = node
        self.tail = node
    end
    self.count = self.count + 1
end

function SinglyLinkedList:get(index)
    if index < 1 or index > self.count then
        error("Index out of bounds")
    end
    local current = self.head
    for _ = 1, index - 1 do
        current = current.next
    end
    return current.data
end

function SinglyLinkedList:set(index, item)
    if index < 1 or index > self.count then
        error("Index out of bounds")
    end
    local current = self.head
    for _ = 1, index - 1 do
        current = current.next
    end
    current.data = item
end

function SinglyLinkedList:removeFirst()
    if self.head == nil then
        error("List is empty")
    end
    local item = self.head.data
    self.head = self.head.next
    if self.head == nil then
        self.tail = nil
    end
    self.count = self.count - 1
    return item
end

function SinglyLinkedList:removeAt(index)
    if index < 1 or index > self.count then
        error("Index out of bounds")
    end
    if index == 1 then
        return self:removeFirst()
    end
    local current = self.head
    for _ = 1, index - 2 do
        current = current.next
    end
    local item = current.next.data
    current.next = current.next.next
    if current.next == nil then
        self.tail = current
    end
    self.count = self.count - 1
    return item
end

function SinglyLinkedList:find(item)
    local current = self.head
    local index = 1
    while current ~= nil do
        if current.data == item then
            return index
        end
        current = current.next
        index = index + 1
    end
    return nil
end

function SinglyLinkedList:contains(item)
    return self:find(item) ~= nil
end

function SinglyLinkedList:isEmpty()
    return self.count == 0
end

function SinglyLinkedList:size()
    return self.count
end

function SinglyLinkedList:clear()
    self.head = nil
    self.tail = nil
    self.count = 0
end

function SinglyLinkedList:toArray()
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

return SinglyLinkedList
