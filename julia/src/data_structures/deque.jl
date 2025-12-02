"""
Deque - Double-ended queue

Time Complexity:
- push_front!: O(1) amortized
- push_back!: O(1) amortized
- pop_front!: O(1) amortized
- pop_back!: O(1)
"""

mutable struct Deque{T}
    items::Vector{T}
    front::Int
    back::Int
    capacity::Int

    function Deque{T}(capacity::Int=16) where T
        items = Vector{T}(undef, capacity)
        new{T}(items, 1, 0, capacity)
    end
end

Deque() = Deque{Any}()

function _resize!(d::Deque{T}) where T
    new_capacity = d.capacity * 2
    new_items = Vector{T}(undef, new_capacity)

    len = length(d)
    for i in 1:len
        idx = ((d.front - 1 + i - 1) % d.capacity) + 1
        new_items[i] = d.items[idx]
    end

    d.items = new_items
    d.front = 1
    d.back = len
    d.capacity = new_capacity
end

"""
    push_front!(d::Deque{T}, item::T) where T

Add an item to the front.
"""
function push_front!(d::Deque{T}, item::T) where T
    if length(d) >= d.capacity - 1
        _resize!(d)
    end
    d.front = ((d.front - 2 + d.capacity) % d.capacity) + 1
    d.items[d.front] = item
    if d.back == 0
        d.back = d.front
    end
    d
end

"""
    push_back!(d::Deque{T}, item::T) where T

Add an item to the back.
"""
function push_back!(d::Deque{T}, item::T) where T
    if length(d) >= d.capacity - 1
        _resize!(d)
    end
    d.back = (d.back % d.capacity) + 1
    d.items[d.back] = item
    d
end

"""
    pop_front!(d::Deque{T}) where T

Remove and return the front item.
"""
function pop_front!(d::Deque{T}) where T
    if isempty(d)
        throw(ArgumentError("Deque is empty"))
    end
    result = d.items[d.front]
    if d.front == d.back
        d.back = 0
    else
        d.front = (d.front % d.capacity) + 1
    end
    result
end

"""
    pop_back!(d::Deque{T}) where T

Remove and return the back item.
"""
function pop_back!(d::Deque{T}) where T
    if isempty(d)
        throw(ArgumentError("Deque is empty"))
    end
    result = d.items[d.back]
    if d.front == d.back
        d.back = 0
    else
        d.back = ((d.back - 2 + d.capacity) % d.capacity) + 1
    end
    result
end

"""
    peek_front(d::Deque{T}) where T

Return the front item without removing it.
"""
function peek_front(d::Deque{T}) where T
    if isempty(d)
        throw(ArgumentError("Deque is empty"))
    end
    d.items[d.front]
end

"""
    peek_back(d::Deque{T}) where T

Return the back item without removing it.
"""
function peek_back(d::Deque{T}) where T
    if isempty(d)
        throw(ArgumentError("Deque is empty"))
    end
    d.items[d.back]
end

"""
    isempty(d::Deque)

Check if the deque is empty.
"""
Base.isempty(d::Deque) = d.back == 0

"""
    length(d::Deque)

Return the number of items.
"""
function Base.length(d::Deque)
    if d.back == 0
        return 0
    end
    if d.back >= d.front
        return d.back - d.front + 1
    end
    return d.capacity - d.front + d.back + 1
end

"""
    size(d::Deque)

Return the number of items.
"""
Base.size(d::Deque) = length(d)

"""
    empty!(d::Deque)

Remove all items.
"""
function Base.empty!(d::Deque)
    d.front = 1
    d.back = 0
    d
end

"""
    collect(d::Deque{T}) where T

Convert to array.
"""
function Base.collect(d::Deque{T}) where T
    result = T[]
    if d.back == 0
        return result
    end
    len = length(d)
    for i in 1:len
        idx = ((d.front - 1 + i - 1) % d.capacity) + 1
        push!(result, d.items[idx])
    end
    result
end
