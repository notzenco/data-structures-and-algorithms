-- Breadth-First Search - Level-order graph traversal
--
-- Time Complexity: O(V + E)
-- Space Complexity: O(V)

local bfs = {}

function bfs.traverse(graph, start)
    local result = {}
    if not graph:hasVertex(start) then
        return result
    end

    local visited = {}
    local queue = { start }
    local front = 1
    visited[start] = true

    while front <= #queue do
        local vertex = queue[front]
        front = front + 1
        result[#result + 1] = vertex

        for _, neighbor in ipairs(graph:neighbors(vertex)) do
            if not visited[neighbor] then
                visited[neighbor] = true
                queue[#queue + 1] = neighbor
            end
        end
    end

    return result
end

function bfs.shortestPath(graph, start, target)
    if not graph:hasVertex(start) or not graph:hasVertex(target) then
        return {}
    end

    if start == target then
        return { start }
    end

    local visited = {}
    local parent = {}
    local queue = { start }
    local front = 1
    visited[start] = true

    while front <= #queue do
        local vertex = queue[front]
        front = front + 1

        if vertex == target then
            local path = {}
            local current = target
            while current ~= start do
                table.insert(path, 1, current)
                current = parent[current]
            end
            table.insert(path, 1, start)
            return path
        end

        for _, neighbor in ipairs(graph:neighbors(vertex)) do
            if not visited[neighbor] then
                visited[neighbor] = true
                parent[neighbor] = vertex
                queue[#queue + 1] = neighbor
            end
        end
    end

    return {}
end

function bfs.distances(graph, start)
    local result = {}
    if not graph:hasVertex(start) then
        return result
    end

    local queue = { start }
    local front = 1
    result[start] = 0

    while front <= #queue do
        local vertex = queue[front]
        front = front + 1
        local dist = result[vertex]

        for _, neighbor in ipairs(graph:neighbors(vertex)) do
            if result[neighbor] == nil then
                result[neighbor] = dist + 1
                queue[#queue + 1] = neighbor
            end
        end
    end

    return result
end

function bfs.pathExists(graph, start, target)
    if not graph:hasVertex(start) or not graph:hasVertex(target) then
        return false
    end

    if start == target then
        return true
    end

    local visited = {}
    local queue = { start }
    local front = 1
    visited[start] = true

    while front <= #queue do
        local vertex = queue[front]
        front = front + 1

        for _, neighbor in ipairs(graph:neighbors(vertex)) do
            if neighbor == target then
                return true
            end
            if not visited[neighbor] then
                visited[neighbor] = true
                queue[#queue + 1] = neighbor
            end
        end
    end

    return false
end

function bfs.connectedComponents(graph)
    local result = {}
    local visited = {}

    for _, vertex in ipairs(graph:vertices()) do
        if not visited[vertex] then
            local component = {}
            local queue = { vertex }
            local front = 1
            visited[vertex] = true

            while front <= #queue do
                local v = queue[front]
                front = front + 1
                component[#component + 1] = v

                for _, neighbor in ipairs(graph:neighbors(v)) do
                    if not visited[neighbor] then
                        visited[neighbor] = true
                        queue[#queue + 1] = neighbor
                    end
                end
            end

            result[#result + 1] = component
        end
    end

    return result
end

function bfs.isConnected(graph)
    if graph:isEmpty() then
        return true
    end

    local vertices = graph:vertices()
    if #vertices == 0 then
        return true
    end

    local traversed = bfs.traverse(graph, vertices[1])
    return #traversed == graph:vertexCount()
end

return bfs
