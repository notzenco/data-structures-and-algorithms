-- Hash Table - Open addressing with linear probing
--
-- Time Complexity (average):
-- - put: O(1)
-- - get: O(1)
-- - remove: O(1)
-- - contains: O(1)

local HashTable = {}
HashTable.__index = HashTable

local EMPTY = 0
local OCCUPIED = 1
local DELETED = 2

local function hashString(s)
    local h = 0
    for i = 1, #s do
        h = (h * 31 + string.byte(s, i)) % 2147483647
    end
    return h
end

local function hashValue(v)
    local t = type(v)
    if t == "string" then
        return hashString(v)
    elseif t == "number" then
        return math.floor(v) % 2147483647
    else
        return hashString(tostring(v))
    end
end

function HashTable.new(capacity)
    local self = setmetatable({}, HashTable)
    self.capacity = capacity or 16
    self.buckets = {}
    self.count = 0
    for i = 1, self.capacity do
        self.buckets[i] = { state = EMPTY }
    end
    return self
end

function HashTable:_hashIndex(key)
    return (hashValue(key) % self.capacity) + 1
end

function HashTable:_findSlot(key)
    local index = self:_hashIndex(key)
    local firstDeleted = nil

    for _ = 1, self.capacity do
        local entry = self.buckets[index]
        if entry.state == EMPTY then
            return firstDeleted or index
        elseif entry.state == DELETED then
            if firstDeleted == nil then
                firstDeleted = index
            end
        elseif entry.key == key then
            return index
        end
        index = (index % self.capacity) + 1
    end

    return firstDeleted or 0
end

function HashTable:_resize()
    local oldBuckets = self.buckets
    local oldCapacity = self.capacity
    self.capacity = self.capacity * 2
    self.buckets = {}
    self.count = 0
    for i = 1, self.capacity do
        self.buckets[i] = { state = EMPTY }
    end
    for i = 1, oldCapacity do
        if oldBuckets[i].state == OCCUPIED then
            self:put(oldBuckets[i].key, oldBuckets[i].value)
        end
    end
end

function HashTable:put(key, value)
    if (self.count + 1) / self.capacity > 0.75 then
        self:_resize()
    end
    local index = self:_findSlot(key)
    if self.buckets[index].state ~= OCCUPIED then
        self.count = self.count + 1
    end
    self.buckets[index] = { key = key, value = value, state = OCCUPIED }
end

function HashTable:get(key)
    local index = self:_hashIndex(key)
    for _ = 1, self.capacity do
        local entry = self.buckets[index]
        if entry.state == EMPTY then
            return nil
        elseif entry.state == OCCUPIED and entry.key == key then
            return entry.value
        end
        index = (index % self.capacity) + 1
    end
    return nil
end

function HashTable:contains(key)
    return self:get(key) ~= nil
end

function HashTable:remove(key)
    local index = self:_hashIndex(key)
    for _ = 1, self.capacity do
        local entry = self.buckets[index]
        if entry.state == EMPTY then
            return false
        elseif entry.state == OCCUPIED and entry.key == key then
            self.buckets[index].state = DELETED
            self.count = self.count - 1
            return true
        end
        index = (index % self.capacity) + 1
    end
    return false
end

function HashTable:isEmpty()
    return self.count == 0
end

function HashTable:size()
    return self.count
end

function HashTable:clear()
    self.buckets = {}
    self.count = 0
    for i = 1, self.capacity do
        self.buckets[i] = { state = EMPTY }
    end
end

function HashTable:keys()
    local result = {}
    for i = 1, self.capacity do
        if self.buckets[i].state == OCCUPIED then
            result[#result + 1] = self.buckets[i].key
        end
    end
    return result
end

function HashTable:values()
    local result = {}
    for i = 1, self.capacity do
        if self.buckets[i].state == OCCUPIED then
            result[#result + 1] = self.buckets[i].value
        end
    end
    return result
end

return HashTable
