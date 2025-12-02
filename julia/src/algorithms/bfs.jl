"""
Breadth-First Search - Level-order graph traversal

Time Complexity: O(V + E)
Space Complexity: O(V)
"""

"""
    bfs_traverse(g::Graph{T}, start::T) where T

Traverse graph starting from vertex using BFS.
Returns vertices in BFS order.
"""
function bfs_traverse(g::Graph{T}, start::T) where T
    result = T[]
    if !has_vertex(g, start)
        return result
    end

    visited = Set{T}()
    queue = T[start]
    push!(visited, start)

    while !isempty(queue)
        vertex = popfirst!(queue)
        push!(result, vertex)

        for neighbor in neighbors(g, vertex)
            if neighbor ∉ visited
                push!(visited, neighbor)
                push!(queue, neighbor)
            end
        end
    end

    result
end

"""
    shortest_path(g::Graph{T}, start::T, target::T) where T

Find shortest path between start and target.
Returns empty vector if no path exists.
"""
function shortest_path(g::Graph{T}, start::T, target::T) where T
    if !has_vertex(g, start) || !has_vertex(g, target)
        return T[]
    end

    if start == target
        return T[start]
    end

    visited = Set{T}()
    parent = Dict{T, T}()
    queue = T[start]
    push!(visited, start)

    while !isempty(queue)
        vertex = popfirst!(queue)

        if vertex == target
            # Reconstruct path
            path = T[]
            current = target
            while current != start
                pushfirst!(path, current)
                current = parent[current]
            end
            pushfirst!(path, start)
            return path
        end

        for neighbor in neighbors(g, vertex)
            if neighbor ∉ visited
                push!(visited, neighbor)
                parent[neighbor] = vertex
                push!(queue, neighbor)
            end
        end
    end

    T[] # No path found
end

"""
    distances(g::Graph{T}, start::T) where T

Calculate distances from start to all reachable vertices.
Returns Dict mapping vertex to distance.
"""
function distances(g::Graph{T}, start::T) where T
    result = Dict{T, Int}()
    if !has_vertex(g, start)
        return result
    end

    queue = T[start]
    result[start] = 0

    while !isempty(queue)
        vertex = popfirst!(queue)
        dist = result[vertex]

        for neighbor in neighbors(g, vertex)
            if !haskey(result, neighbor)
                result[neighbor] = dist + 1
                push!(queue, neighbor)
            end
        end
    end

    result
end

"""
    path_exists(g::Graph{T}, start::T, target::T) where T

Check if path exists between start and target.
"""
function path_exists(g::Graph{T}, start::T, target::T) where T
    if !has_vertex(g, start) || !has_vertex(g, target)
        return false
    end

    if start == target
        return true
    end

    visited = Set{T}()
    queue = T[start]
    push!(visited, start)

    while !isempty(queue)
        vertex = popfirst!(queue)

        for neighbor in neighbors(g, vertex)
            if neighbor == target
                return true
            end
            if neighbor ∉ visited
                push!(visited, neighbor)
                push!(queue, neighbor)
            end
        end
    end

    false
end

"""
    connected_components(g::Graph{T}) where T

Find all connected components in undirected graph.
"""
function connected_components(g::Graph{T}) where T
    result = Vector{Vector{T}}()
    visited = Set{T}()

    for vertex in vertices(g)
        if vertex ∉ visited
            component = T[]
            queue = T[vertex]
            push!(visited, vertex)

            while !isempty(queue)
                v = popfirst!(queue)
                push!(component, v)

                for neighbor in neighbors(g, v)
                    if neighbor ∉ visited
                        push!(visited, neighbor)
                        push!(queue, neighbor)
                    end
                end
            end

            push!(result, component)
        end
    end

    result
end

"""
    is_connected(g::Graph{T}) where T

Check if graph is connected (all vertices reachable from any vertex).
"""
function is_connected(g::Graph{T}) where T
    if isempty(g)
        return true
    end

    all_vertices = vertices(g)
    if isempty(all_vertices)
        return true
    end

    traversed = bfs_traverse(g, all_vertices[1])
    length(traversed) == vertex_count(g)
end
