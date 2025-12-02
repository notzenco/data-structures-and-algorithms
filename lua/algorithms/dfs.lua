-- Depth-First Search - Graph traversal algorithm
--
-- Time Complexity: O(V + E)
-- Space Complexity: O(V)

local dfs = {}

function dfs.traverse(graph, start)
    local result = {}
    if not graph:hasVertex(start) then
        return result
    end

    local visited = {}
    local stack = { start }

    while #stack > 0 do
        local vertex = table.remove(stack)

        if not visited[vertex] then
            visited[vertex] = true
            result[#result + 1] = vertex

            for _, neighbor in ipairs(graph:neighbors(vertex)) do
                if not visited[neighbor] then
                    stack[#stack + 1] = neighbor
                end
            end
        end
    end

    return result
end

function dfs.traverseRecursive(graph, start)
    local result = {}
    if not graph:hasVertex(start) then
        return result
    end

    local visited = {}

    local function visit(vertex)
        visited[vertex] = true
        result[#result + 1] = vertex
        for _, neighbor in ipairs(graph:neighbors(vertex)) do
            if not visited[neighbor] then
                visit(neighbor)
            end
        end
    end

    visit(start)
    return result
end

function dfs.findPath(graph, start, target)
    if not graph:hasVertex(start) or not graph:hasVertex(target) then
        return {}
    end

    if start == target then
        return { start }
    end

    local visited = {}
    local parent = {}
    local stack = { start }

    while #stack > 0 do
        local vertex = table.remove(stack)

        if not visited[vertex] then
            visited[vertex] = true

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
                    parent[neighbor] = vertex
                    stack[#stack + 1] = neighbor
                end
            end
        end
    end

    return {}
end

function dfs.hasCycle(graph)
    if not graph:isDirected() then
        -- For undirected graphs
        local visited = {}

        local function hasCycleUndirected(vertex, parentVertex)
            visited[vertex] = true
            for _, neighbor in ipairs(graph:neighbors(vertex)) do
                if not visited[neighbor] then
                    if hasCycleUndirected(neighbor, vertex) then
                        return true
                    end
                elseif neighbor ~= parentVertex then
                    return true
                end
            end
            return false
        end

        for _, v in ipairs(graph:vertices()) do
            if not visited[v] then
                if hasCycleUndirected(v, v) then
                    return true
                end
            end
        end
        return false
    end

    -- For directed graphs
    local visited = {}
    local recStack = {}

    local function hasCycleUtil(vertex)
        visited[vertex] = true
        recStack[vertex] = true

        for _, neighbor in ipairs(graph:neighbors(vertex)) do
            if not visited[neighbor] then
                if hasCycleUtil(neighbor) then
                    return true
                end
            elseif recStack[neighbor] then
                return true
            end
        end

        recStack[vertex] = nil
        return false
    end

    for _, v in ipairs(graph:vertices()) do
        if not visited[v] then
            if hasCycleUtil(v) then
                return true
            end
        end
    end

    return false
end

function dfs.topologicalSort(graph)
    if not graph:isDirected() then
        error("Topological sort requires directed graph")
    end

    local visited = {}
    local recStack = {}
    local sorted = {}
    local hasCycleFlag = false

    local function visit(vertex)
        if hasCycleFlag then
            return
        end
        if recStack[vertex] then
            hasCycleFlag = true
            return
        end
        if visited[vertex] then
            return
        end

        recStack[vertex] = true
        for _, neighbor in ipairs(graph:neighbors(vertex)) do
            visit(neighbor)
        end
        recStack[vertex] = nil
        visited[vertex] = true
        table.insert(sorted, 1, vertex)
    end

    for _, v in ipairs(graph:vertices()) do
        if not visited[v] then
            visit(v)
        end
    end

    if hasCycleFlag then
        return {}
    end
    return sorted
end

function dfs.allPaths(graph, start, target)
    local result = {}
    if not graph:hasVertex(start) or not graph:hasVertex(target) then
        return result
    end

    local path = {}
    local visited = {}

    local function findAllPaths(current)
        visited[current] = true
        path[#path + 1] = current

        if current == target then
            local pathCopy = {}
            for i, v in ipairs(path) do
                pathCopy[i] = v
            end
            result[#result + 1] = pathCopy
        else
            for _, neighbor in ipairs(graph:neighbors(current)) do
                if not visited[neighbor] then
                    findAllPaths(neighbor)
                end
            end
        end

        table.remove(path)
        visited[current] = nil
    end

    findAllPaths(start)
    return result
end

return dfs
