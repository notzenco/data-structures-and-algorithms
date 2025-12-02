-- Disjoint Set (Union-Find) with path compression and union by rank
--
-- Time Complexity:
-- - makeSet: O(1)
-- - find: O(α(n)) amortized (nearly constant)
-- - union: O(α(n)) amortized (nearly constant)
-- - connected: O(α(n)) amortized

local DisjointSet = {}
DisjointSet.__index = DisjointSet

function DisjointSet.new()
    local self = setmetatable({}, DisjointSet)
    self.parent = {}
    self.rank = {}
    self.count = 0
    return self
end

function DisjointSet:makeSet(x)
    if self.parent[x] == nil then
        self.parent[x] = x
        self.rank[x] = 0
        self.count = self.count + 1
    end
end

function DisjointSet:find(x)
    if self.parent[x] == nil then
        error("Element not in any set")
    end
    if self.parent[x] ~= x then
        self.parent[x] = self:find(self.parent[x])
    end
    return self.parent[x]
end

function DisjointSet:union(x, y)
    local rootX = self:find(x)
    local rootY = self:find(y)

    if rootX == rootY then
        return false
    end

    local rankX = self.rank[rootX]
    local rankY = self.rank[rootY]

    if rankX < rankY then
        self.parent[rootX] = rootY
    elseif rankX > rankY then
        self.parent[rootY] = rootX
    else
        self.parent[rootY] = rootX
        self.rank[rootX] = rankX + 1
    end

    return true
end

function DisjointSet:connected(x, y)
    return self:find(x) == self:find(y)
end

function DisjointSet:contains(x)
    return self.parent[x] ~= nil
end

function DisjointSet:setCount()
    local roots = {}
    for x, _ in pairs(self.parent) do
        local root = self:find(x)
        roots[root] = true
    end
    local count = 0
    for _, _ in pairs(roots) do
        count = count + 1
    end
    return count
end

function DisjointSet:size()
    return self.count
end

function DisjointSet:clear()
    self.parent = {}
    self.rank = {}
    self.count = 0
end

function DisjointSet:getSetMembers(x)
    local root = self:find(x)
    local result = {}
    for element, _ in pairs(self.parent) do
        if self:find(element) == root then
            result[#result + 1] = element
        end
    end
    return result
end

return DisjointSet
