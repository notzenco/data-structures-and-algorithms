"""
Queue - FIFO data structure

Time Complexity:
- enqueue!: O(1) amortized
- dequeue!: O(1) amortized
- peek: O(1)
- isempty: O(1)
- size: O(1)
"""

mutable struct Queue{T}
    items::Vector{T}
    front::Int

    Queue{T}() where T = new{T}(T[], 1)
end

Queue() = Queue{Any}()

"""
    enqueue!(q::Queue{T}, item::T) where T

Add an item to the back of the queue.
"""
function enqueue!(q::Queue{T}, item::T) where T
    push!(q.items, item)
    q
end

"""
    dequeue!(q::Queue{T}) where T

Remove and return the front item.
Throws ArgumentError if queue is empty.
"""
function dequeue!(q::Queue{T}) where T
    if isempty(q)
        throw(ArgumentError("Queue is empty"))
    end
    result = q.items[q.front]
    q.front += 1

    # Compact if too much wasted space
    if q.front > length(q.items) ÷ 2 && q.front > 16
        q.items = q.items[q.front:end]
        q.front = 1
    end

    result
end

"""
    peek(q::Queue{T}) where T

Return the front item without removing it.
Throws ArgumentError if queue is empty.
"""
function peek(q::Queue{T}) where T
    if isempty(q)
        throw(ArgumentError("Queue is empty"))
    end
    q.items[q.front]
end

"""
    isempty(q::Queue)

Check if the queue is empty.
"""
Base.isempty(q::Queue) = q.front > length(q.items)

"""
    size(q::Queue)

Return the number of items in the queue.
"""
Base.size(q::Queue) = length(q.items) - q.front + 1

"""
    length(q::Queue)

Return the number of items in the queue.
"""
Base.length(q::Queue) = max(0, length(q.items) - q.front + 1)

"""
    empty!(q::Queue)

Remove all items from the queue.
"""
function Base.empty!(q::Queue)
    empty!(q.items)
    q.front = 1
    q
end

"""
    collect(q::Queue)

Convert queue to array (front to back).
"""
function Base.collect(q::Queue)
    if isempty(q)
        return eltype(q.items)[]
    end
    q.items[q.front:end]
end
