-- Min Heap - Binary min heap / priority queue
--
-- Time Complexity:
-- - insert: O(log n)
-- - extractMin: O(log n)
-- - peek: O(1)

local MinHeap = {}
MinHeap.__index = MinHeap

local function parent(i) return math.floor((i - 1) / 2) + 1 end
local function leftChild(i) return 2 * i end
local function rightChild(i) return 2 * i + 1 end

function MinHeap.new()
    local self = setmetatable({}, MinHeap)
    self.items = {}
    self.count = 0
    return self
end

local function siftUp(heap, index)
    while index > 1 and heap.items[parent(index)] > heap.items[index] do
        local p = parent(index)
        heap.items[p], heap.items[index] = heap.items[index], heap.items[p]
        index = p
    end
end

local function siftDown(heap, index)
    local smallest = index
    local left = leftChild(index)
    local right = rightChild(index)

    if left <= heap.count and heap.items[left] < heap.items[smallest] then
        smallest = left
    end
    if right <= heap.count and heap.items[right] < heap.items[smallest] then
        smallest = right
    end

    if smallest ~= index then
        heap.items[index], heap.items[smallest] = heap.items[smallest], heap.items[index]
        siftDown(heap, smallest)
    end
end

function MinHeap:insert(item)
    self.count = self.count + 1
    self.items[self.count] = item
    siftUp(self, self.count)
end

function MinHeap:push(item)
    self:insert(item)
end

function MinHeap:extractMin()
    if self.count == 0 then
        error("Heap is empty")
    end
    local min = self.items[1]
    self.items[1] = self.items[self.count]
    self.items[self.count] = nil
    self.count = self.count - 1
    if self.count > 0 then
        siftDown(self, 1)
    end
    return min
end

function MinHeap:pop()
    return self:extractMin()
end

function MinHeap:peek()
    if self.count == 0 then
        error("Heap is empty")
    end
    return self.items[1]
end

function MinHeap:isEmpty()
    return self.count == 0
end

function MinHeap:size()
    return self.count
end

function MinHeap:clear()
    self.items = {}
    self.count = 0
end

function MinHeap.heapify(arr)
    local heap = MinHeap.new()
    heap.items = {}
    for i = 1, #arr do
        heap.items[i] = arr[i]
    end
    heap.count = #arr
    for i = math.floor(heap.count / 2), 1, -1 do
        siftDown(heap, i)
    end
    return heap
end

function MinHeap:toArray()
    local result = {}
    for i = 1, self.count do
        result[i] = self.items[i]
    end
    return result
end

return MinHeap
