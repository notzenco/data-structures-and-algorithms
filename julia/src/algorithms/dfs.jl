"""
Depth-First Search - Graph traversal algorithm

Time Complexity: O(V + E)
Space Complexity: O(V)
"""

"""
    dfs_traverse(g::Graph{T}, start::T) where T

Traverse graph starting from vertex using DFS (iterative).
Returns vertices in DFS order.
"""
function dfs_traverse(g::Graph{T}, start::T) where T
    result = T[]
    if !has_vertex(g, start)
        return result
    end

    visited = Set{T}()
    stack = T[start]

    while !isempty(stack)
        vertex = pop!(stack)

        if vertex ∈ visited
            continue
        end

        push!(visited, vertex)
        push!(result, vertex)

        for neighbor in neighbors(g, vertex)
            if neighbor ∉ visited
                push!(stack, neighbor)
            end
        end
    end

    result
end

"""
    dfs_traverse_recursive(g::Graph{T}, start::T) where T

Traverse graph using recursive DFS.
"""
function dfs_traverse_recursive(g::Graph{T}, start::T) where T
    result = T[]
    if !has_vertex(g, start)
        return result
    end

    visited = Set{T}()

    function dfs(vertex::T)
        push!(visited, vertex)
        push!(result, vertex)
        for neighbor in neighbors(g, vertex)
            if neighbor ∉ visited
                dfs(neighbor)
            end
        end
    end

    dfs(start)
    result
end

"""
    find_path(g::Graph{T}, start::T, target::T) where T

Find a path from start to target using DFS.
Returns empty vector if no path exists.
"""
function find_path(g::Graph{T}, start::T, target::T) where T
    if !has_vertex(g, start) || !has_vertex(g, target)
        return T[]
    end

    if start == target
        return T[start]
    end

    visited = Set{T}()
    parent = Dict{T, T}()
    stack = T[start]

    while !isempty(stack)
        vertex = pop!(stack)

        if vertex ∈ visited
            continue
        end

        push!(visited, vertex)

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
                parent[neighbor] = vertex
                push!(stack, neighbor)
            end
        end
    end

    T[] # No path found
end

"""
    has_cycle(g::Graph{T}) where T

Check if graph has a cycle.
"""
function has_cycle(g::Graph{T}) where T
    if !is_directed(g)
        # For undirected graphs
        visited = Set{T}()

        function has_cycle_undirected(vertex::T, parent::T)::Bool
            push!(visited, vertex)
            for neighbor in neighbors(g, vertex)
                if neighbor ∉ visited
                    if has_cycle_undirected(neighbor, vertex)
                        return true
                    end
                elseif neighbor != parent
                    return true
                end
            end
            false
        end

        for v in vertices(g)
            if v ∉ visited
                if has_cycle_undirected(v, v)
                    return true
                end
            end
        end
        return false
    end

    # For directed graphs
    visited = Set{T}()
    rec_stack = Set{T}()

    function has_cycle_util(vertex::T)::Bool
        push!(visited, vertex)
        push!(rec_stack, vertex)

        for neighbor in neighbors(g, vertex)
            if neighbor ∉ visited
                if has_cycle_util(neighbor)
                    return true
                end
            elseif neighbor ∈ rec_stack
                return true
            end
        end

        delete!(rec_stack, vertex)
        false
    end

    for v in vertices(g)
        if v ∉ visited
            if has_cycle_util(v)
                return true
            end
        end
    end

    false
end

"""
    topological_sort(g::Graph{T}) where T

Topological sort for directed acyclic graph.
Returns empty vector if graph has a cycle.
"""
function topological_sort(g::Graph{T}) where T
    if !is_directed(g)
        throw(ArgumentError("Topological sort requires directed graph"))
    end

    visited = Set{T}()
    rec_stack = Set{T}()
    sorted = T[]
    has_cycle_flag = Ref(false)

    function visit(vertex::T)
        if has_cycle_flag[]
            return
        end
        if vertex ∈ rec_stack
            has_cycle_flag[] = true
            return
        end
        if vertex ∈ visited
            return
        end

        push!(rec_stack, vertex)
        for neighbor in neighbors(g, vertex)
            visit(neighbor)
        end
        delete!(rec_stack, vertex)
        push!(visited, vertex)
        pushfirst!(sorted, vertex)
    end

    for v in vertices(g)
        if v ∉ visited
            visit(v)
        end
    end

    if has_cycle_flag[]
        return T[]
    end
    sorted
end

"""
    all_paths(g::Graph{T}, start::T, target::T) where T

Find all paths from start to target.
"""
function all_paths(g::Graph{T}, start::T, target::T) where T
    result = Vector{Vector{T}}()
    if !has_vertex(g, start) || !has_vertex(g, target)
        return result
    end

    path = T[]
    visited = Set{T}()

    function find_all_paths(current::T)
        push!(visited, current)
        push!(path, current)

        if current == target
            push!(result, copy(path))
        else
            for neighbor in neighbors(g, current)
                if neighbor ∉ visited
                    find_all_paths(neighbor)
                end
            end
        end

        pop!(path)
        delete!(visited, current)
    end

    find_all_paths(start)
    result
end
