"""
Stack - LIFO data structure

Time Complexity:
- push!: O(1) amortized
- pop!: O(1)
- peek: O(1)
- isempty: O(1)
- size: O(1)
"""

mutable struct Stack{T}
    items::Vector{T}

    Stack{T}() where T = new{T}(T[])
end

Stack() = Stack{Any}()

"""
    push!(s::Stack{T}, item::T) where T

Push an item onto the stack.
"""
function Base.push!(s::Stack{T}, item::T) where T
    push!(s.items, item)
    s
end

"""
    pop!(s::Stack{T}) where T

Remove and return the top item.
Throws ArgumentError if stack is empty.
"""
function Base.pop!(s::Stack{T}) where T
    if isempty(s)
        throw(ArgumentError("Stack is empty"))
    end
    pop!(s.items)
end

"""
    peek(s::Stack{T}) where T

Return the top item without removing it.
Throws ArgumentError if stack is empty.
"""
function peek(s::Stack{T}) where T
    if isempty(s)
        throw(ArgumentError("Stack is empty"))
    end
    s.items[end]
end

"""
    isempty(s::Stack)

Check if the stack is empty.
"""
Base.isempty(s::Stack) = isempty(s.items)

"""
    size(s::Stack)

Return the number of items in the stack.
"""
Base.size(s::Stack) = length(s.items)

"""
    length(s::Stack)

Return the number of items in the stack.
"""
Base.length(s::Stack) = length(s.items)

"""
    empty!(s::Stack)

Remove all items from the stack.
"""
function Base.empty!(s::Stack)
    empty!(s.items)
    s
end

"""
    collect(s::Stack)

Convert stack to array (bottom to top).
"""
Base.collect(s::Stack) = copy(s.items)

# Iterator interface
Base.iterate(s::Stack, state=1) = state > length(s.items) ? nothing : (s.items[state], state + 1)
Base.eltype(::Type{Stack{T}}) where T = T
