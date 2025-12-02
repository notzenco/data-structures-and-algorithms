-- Graph - Adjacency list representation
--
-- Supports both directed and undirected graphs

local Graph = {}
Graph.__index = Graph

function Graph.new(directed)
    local self = setmetatable({}, Graph)
    self.adjacencyList = {}
    self.directed = directed or false
    return self
end

function Graph:addVertex(vertex)
    if self.adjacencyList[vertex] == nil then
        self.adjacencyList[vertex] = {}
    end
end

function Graph:addEdge(fromVertex, toVertex)
    self:addVertex(fromVertex)
    self:addVertex(toVertex)
    self.adjacencyList[fromVertex][toVertex] = true
    if not self.directed then
        self.adjacencyList[toVertex][fromVertex] = true
    end
end

function Graph:removeEdge(fromVertex, toVertex)
    if self.adjacencyList[fromVertex] == nil then
        return false
    end
    if not self.adjacencyList[fromVertex][toVertex] then
        return false
    end
    self.adjacencyList[fromVertex][toVertex] = nil
    if not self.directed and self.adjacencyList[toVertex] then
        self.adjacencyList[toVertex][fromVertex] = nil
    end
    return true
end

function Graph:removeVertex(vertex)
    if self.adjacencyList[vertex] == nil then
        return false
    end
    self.adjacencyList[vertex] = nil
    for v, neighbors in pairs(self.adjacencyList) do
        neighbors[vertex] = nil
    end
    return true
end

function Graph:hasVertex(vertex)
    return self.adjacencyList[vertex] ~= nil
end

function Graph:hasEdge(fromVertex, toVertex)
    if self.adjacencyList[fromVertex] == nil then
        return false
    end
    return self.adjacencyList[fromVertex][toVertex] == true
end

function Graph:neighbors(vertex)
    local result = {}
    if self.adjacencyList[vertex] then
        for neighbor, _ in pairs(self.adjacencyList[vertex]) do
            result[#result + 1] = neighbor
        end
    end
    return result
end

function Graph:vertices()
    local result = {}
    for vertex, _ in pairs(self.adjacencyList) do
        result[#result + 1] = vertex
    end
    return result
end

function Graph:vertexCount()
    local count = 0
    for _, _ in pairs(self.adjacencyList) do
        count = count + 1
    end
    return count
end

function Graph:edgeCount()
    local count = 0
    for _, neighbors in pairs(self.adjacencyList) do
        for _, _ in pairs(neighbors) do
            count = count + 1
        end
    end
    if not self.directed then
        count = count / 2
    end
    return count
end

function Graph:isDirected()
    return self.directed
end

function Graph:degree(vertex)
    if self.adjacencyList[vertex] == nil then
        return 0
    end
    local count = 0
    for _, _ in pairs(self.adjacencyList[vertex]) do
        count = count + 1
    end
    return count
end

function Graph:isEmpty()
    return next(self.adjacencyList) == nil
end

function Graph:clear()
    self.adjacencyList = {}
end

return Graph
