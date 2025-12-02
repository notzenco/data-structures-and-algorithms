"""
Dynamic Array - Resizable array implementation

Time Complexity:
- get/set!: O(1)
- push!: O(1) amortized
- pop!: O(1)
- insert!: O(n)
- remove_at!: O(n)
"""

mutable struct DynamicArray{T}
    items::Vector{T}

    DynamicArray{T}() where T = new{T}(T[])
    DynamicArray{T}(capacity::Int) where T = new{T}(Vector{T}(undef, 0))
end

DynamicArray() = DynamicArray{Any}()

"""
    push!(arr::DynamicArray{T}, item::T) where T

Add an item to the end.
"""
function Base.push!(arr::DynamicArray{T}, item::T) where T
    push!(arr.items, item)
    arr
end

"""
    pop!(arr::DynamicArray{T}) where T

Remove and return the last item.
Throws ArgumentError if array is empty.
"""
function Base.pop!(arr::DynamicArray{T}) where T
    if isempty(arr)
        throw(ArgumentError("Array is empty"))
    end
    pop!(arr.items)
end

"""
    get(arr::DynamicArray{T}, index::Int) where T

Get item at index (1-based).
Throws BoundsError if index is out of bounds.
"""
function get(arr::DynamicArray{T}, index::Int) where T
    if index < 1 || index > length(arr)
        throw(BoundsError(arr.items, index))
    end
    arr.items[index]
end

"""
    set!(arr::DynamicArray{T}, index::Int, item::T) where T

Set item at index (1-based).
Throws BoundsError if index is out of bounds.
"""
function set!(arr::DynamicArray{T}, index::Int, item::T) where T
    if index < 1 || index > length(arr)
        throw(BoundsError(arr.items, index))
    end
    arr.items[index] = item
    arr
end

# Indexing support
Base.getindex(arr::DynamicArray, i::Int) = get(arr, i)
Base.setindex!(arr::DynamicArray{T}, v::T, i::Int) where T = set!(arr, i, v)

"""
    insert!(arr::DynamicArray{T}, index::Int, item::T) where T

Insert item at index, shifting subsequent items right.
"""
function Base.insert!(arr::DynamicArray{T}, index::Int, item::T) where T
    if index < 1 || index > length(arr) + 1
        throw(BoundsError(arr.items, index))
    end
    insert!(arr.items, index, item)
    arr
end

"""
    remove_at!(arr::DynamicArray{T}, index::Int) where T

Remove and return item at index.
"""
function remove_at!(arr::DynamicArray{T}, index::Int) where T
    if index < 1 || index > length(arr)
        throw(BoundsError(arr.items, index))
    end
    item = arr.items[index]
    deleteat!(arr.items, index)
    item
end

"""
    find(arr::DynamicArray{T}, item::T) where T

Find index of item, returns nothing if not found.
"""
function Base.findfirst(arr::DynamicArray{T}, item::T) where T
    findfirst(==(item), arr.items)
end

"""
    contains(arr::DynamicArray{T}, item::T) where T

Check if array contains item.
"""
contains(arr::DynamicArray{T}, item::T) where T = item in arr.items

"""
    isempty(arr::DynamicArray)

Check if the array is empty.
"""
Base.isempty(arr::DynamicArray) = isempty(arr.items)

"""
    length(arr::DynamicArray)

Return the number of items.
"""
Base.length(arr::DynamicArray) = length(arr.items)

"""
    size(arr::DynamicArray)

Return the number of items.
"""
Base.size(arr::DynamicArray) = length(arr.items)

"""
    empty!(arr::DynamicArray)

Remove all items.
"""
function Base.empty!(arr::DynamicArray)
    empty!(arr.items)
    arr
end

"""
    collect(arr::DynamicArray)

Convert to standard array.
"""
Base.collect(arr::DynamicArray) = copy(arr.items)

# Iterator interface
Base.iterate(arr::DynamicArray, state=1) = iterate(arr.items, state)
Base.eltype(::Type{DynamicArray{T}}) where T = T
