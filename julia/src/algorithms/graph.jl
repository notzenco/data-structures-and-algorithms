"""
Graph - Adjacency list representation

Supports both directed and undirected graphs.
"""

mutable struct Graph{T}
    adjacency_list::Dict{T, Set{T}}
    directed::Bool

    Graph{T}(directed::Bool=false) where T = new{T}(Dict{T, Set{T}}(), directed)
end

Graph(directed::Bool=false) = Graph{Any}(directed)

"""
    add_vertex!(g::Graph{T}, vertex::T) where T

Add a vertex to the graph.
"""
function add_vertex!(g::Graph{T}, vertex::T) where T
    if !haskey(g.adjacency_list, vertex)
        g.adjacency_list[vertex] = Set{T}()
    end
    g
end

"""
    add_edge!(g::Graph{T}, from::T, to::T) where T

Add an edge between two vertices.
"""
function add_edge!(g::Graph{T}, from::T, to::T) where T
    add_vertex!(g, from)
    add_vertex!(g, to)
    push!(g.adjacency_list[from], to)
    if !g.directed
        push!(g.adjacency_list[to], from)
    end
    g
end

"""
    remove_edge!(g::Graph{T}, from::T, to::T) where T

Remove an edge between two vertices.
Returns true if edge existed.
"""
function remove_edge!(g::Graph{T}, from::T, to::T) where T
    if !haskey(g.adjacency_list, from)
        return false
    end
    if to ∉ g.adjacency_list[from]
        return false
    end
    delete!(g.adjacency_list[from], to)
    if !g.directed && haskey(g.adjacency_list, to)
        delete!(g.adjacency_list[to], from)
    end
    true
end

"""
    remove_vertex!(g::Graph{T}, vertex::T) where T

Remove a vertex and all its edges.
Returns true if vertex existed.
"""
function remove_vertex!(g::Graph{T}, vertex::T) where T
    if !haskey(g.adjacency_list, vertex)
        return false
    end
    delete!(g.adjacency_list, vertex)
    for neighbors in values(g.adjacency_list)
        delete!(neighbors, vertex)
    end
    true
end

"""
    has_vertex(g::Graph{T}, vertex::T) where T

Check if vertex exists.
"""
has_vertex(g::Graph{T}, vertex::T) where T = haskey(g.adjacency_list, vertex)

"""
    has_edge(g::Graph{T}, from::T, to::T) where T

Check if edge exists.
"""
function has_edge(g::Graph{T}, from::T, to::T) where T
    if !haskey(g.adjacency_list, from)
        return false
    end
    to ∈ g.adjacency_list[from]
end

"""
    neighbors(g::Graph{T}, vertex::T) where T

Get all neighbors of a vertex.
"""
function neighbors(g::Graph{T}, vertex::T) where T
    if !haskey(g.adjacency_list, vertex)
        return T[]
    end
    collect(g.adjacency_list[vertex])
end

"""
    vertices(g::Graph{T}) where T

Get all vertices.
"""
vertices(g::Graph{T}) where T = collect(keys(g.adjacency_list))

"""
    vertex_count(g::Graph)

Get number of vertices.
"""
vertex_count(g::Graph) = length(g.adjacency_list)

"""
    edge_count(g::Graph)

Get number of edges.
"""
function edge_count(g::Graph)
    count = sum(length(neighbors) for neighbors in values(g.adjacency_list))
    g.directed ? count : count ÷ 2
end

"""
    is_directed(g::Graph)

Check if graph is directed.
"""
is_directed(g::Graph) = g.directed

"""
    degree(g::Graph{T}, vertex::T) where T

Get degree of a vertex.
"""
function degree(g::Graph{T}, vertex::T) where T
    if !haskey(g.adjacency_list, vertex)
        return 0
    end
    length(g.adjacency_list[vertex])
end

"""
    isempty(g::Graph)

Check if graph is empty.
"""
Base.isempty(g::Graph) = isempty(g.adjacency_list)

"""
    empty!(g::Graph)

Remove all vertices and edges.
"""
function Base.empty!(g::Graph)
    empty!(g.adjacency_list)
    g
end
