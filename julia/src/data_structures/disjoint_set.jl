"""
Disjoint Set (Union-Find) with path compression and union by rank

Time Complexity:
- make_set!: O(1)
- find!: O(α(n)) amortized (nearly constant)
- union!: O(α(n)) amortized (nearly constant)
- connected: O(α(n)) amortized (nearly constant)
"""

mutable struct DisjointSet{T}
    parent::Dict{T, T}
    rank::Dict{T, Int}

    DisjointSet{T}() where T = new{T}(Dict{T, T}(), Dict{T, Int}())
end

DisjointSet() = DisjointSet{Any}()

"""
    make_set!(ds::DisjointSet{T}, x::T) where T

Create a new set containing only x.
"""
function make_set!(ds::DisjointSet{T}, x::T) where T
    if !haskey(ds.parent, x)
        ds.parent[x] = x
        ds.rank[x] = 0
    end
    ds
end

"""
    find!(ds::DisjointSet{T}, x::T) where T

Find the representative of the set containing x.
Throws KeyError if x is not in any set.
"""
function find!(ds::DisjointSet{T}, x::T) where T
    if !haskey(ds.parent, x)
        throw(KeyError(x))
    end

    if ds.parent[x] != x
        # Path compression
        ds.parent[x] = find!(ds, ds.parent[x])
    end
    ds.parent[x]
end

"""
    union!(ds::DisjointSet{T}, x::T, y::T) where T

Merge the sets containing x and y.
Returns true if sets were different (and thus merged).
"""
function union!(ds::DisjointSet{T}, x::T, y::T) where T
    root_x = find!(ds, x)
    root_y = find!(ds, y)

    if root_x == root_y
        return false
    end

    # Union by rank
    rank_x = ds.rank[root_x]
    rank_y = ds.rank[root_y]

    if rank_x < rank_y
        ds.parent[root_x] = root_y
    elseif rank_x > rank_y
        ds.parent[root_y] = root_x
    else
        ds.parent[root_y] = root_x
        ds.rank[root_x] = rank_x + 1
    end

    true
end

"""
    connected(ds::DisjointSet{T}, x::T, y::T) where T

Check if x and y are in the same set.
"""
function connected(ds::DisjointSet{T}, x::T, y::T) where T
    find!(ds, x) == find!(ds, y)
end

"""
    contains(ds::DisjointSet{T}, x::T) where T

Check if x is in any set.
"""
contains(ds::DisjointSet{T}, x::T) where T = haskey(ds.parent, x)

"""
    set_count(ds::DisjointSet{T}) where T

Count the number of disjoint sets.
"""
function set_count(ds::DisjointSet{T}) where T
    roots = Set{T}()
    for x in keys(ds.parent)
        push!(roots, find!(ds, x))
    end
    length(roots)
end

"""
    length(ds::DisjointSet)

Return total number of elements.
"""
Base.length(ds::DisjointSet) = length(ds.parent)

"""
    size(ds::DisjointSet)

Return total number of elements.
"""
Base.size(ds::DisjointSet) = length(ds.parent)

"""
    isempty(ds::DisjointSet)

Check if the disjoint set is empty.
"""
Base.isempty(ds::DisjointSet) = isempty(ds.parent)

"""
    empty!(ds::DisjointSet)

Remove all sets.
"""
function Base.empty!(ds::DisjointSet)
    empty!(ds.parent)
    empty!(ds.rank)
    ds
end

"""
    get_set_members(ds::DisjointSet{T}, x::T) where T

Get all members of the set containing x.
"""
function get_set_members(ds::DisjointSet{T}, x::T) where T
    root = find!(ds, x)
    result = T[]
    for element in keys(ds.parent)
        if find!(ds, element) == root
            push!(result, element)
        end
    end
    result
end
