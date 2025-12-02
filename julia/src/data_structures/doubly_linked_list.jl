"""
Doubly Linked List - Bidirectional linked list

Time Complexity:
- prepend!: O(1)
- append!: O(1)
- remove_first!: O(1)
- remove_last!: O(1)
- get: O(n)
"""

mutable struct DLLNode{T}
    data::T
    prev::Union{DLLNode{T}, Nothing}
    next::Union{DLLNode{T}, Nothing}
end

mutable struct DoublyLinkedList{T}
    head::Union{DLLNode{T}, Nothing}
    tail::Union{DLLNode{T}, Nothing}
    count::Int

    DoublyLinkedList{T}() where T = new{T}(nothing, nothing, 0)
end

DoublyLinkedList() = DoublyLinkedList{Any}()

"""
    prepend!(list::DoublyLinkedList{T}, item::T) where T

Add an item to the front.
"""
function prepend!(list::DoublyLinkedList{T}, item::T) where T
    node = DLLNode{T}(item, nothing, list.head)
    if !isnothing(list.head)
        list.head.prev = node
    end
    list.head = node
    if isnothing(list.tail)
        list.tail = node
    end
    list.count += 1
    list
end

"""
    append!(list::DoublyLinkedList{T}, item::T) where T

Add an item to the back.
"""
function Base.append!(list::DoublyLinkedList{T}, item::T) where T
    node = DLLNode{T}(item, list.tail, nothing)
    if !isnothing(list.tail)
        list.tail.next = node
    end
    list.tail = node
    if isnothing(list.head)
        list.head = node
    end
    list.count += 1
    list
end

"""
    get(list::DoublyLinkedList{T}, index::Int) where T

Get item at index (1-based).
"""
function get(list::DoublyLinkedList{T}, index::Int) where T
    if index < 1 || index > list.count
        throw(BoundsError(list, index))
    end
    # Start from closer end
    if index <= list.count ÷ 2
        current = list.head
        for _ in 1:(index-1)
            current = current.next
        end
    else
        current = list.tail
        for _ in 1:(list.count - index)
            current = current.prev
        end
    end
    current.data
end

Base.getindex(list::DoublyLinkedList, i::Int) = get(list, i)

"""
    set!(list::DoublyLinkedList{T}, index::Int, item::T) where T

Set item at index (1-based).
"""
function set!(list::DoublyLinkedList{T}, index::Int, item::T) where T
    if index < 1 || index > list.count
        throw(BoundsError(list, index))
    end
    if index <= list.count ÷ 2
        current = list.head
        for _ in 1:(index-1)
            current = current.next
        end
    else
        current = list.tail
        for _ in 1:(list.count - index)
            current = current.prev
        end
    end
    current.data = item
    list
end

"""
    remove_first!(list::DoublyLinkedList{T}) where T

Remove and return the first item.
"""
function remove_first!(list::DoublyLinkedList{T}) where T
    if isnothing(list.head)
        throw(ArgumentError("List is empty"))
    end
    result = list.head.data
    list.head = list.head.next
    if !isnothing(list.head)
        list.head.prev = nothing
    else
        list.tail = nothing
    end
    list.count -= 1
    result
end

"""
    remove_last!(list::DoublyLinkedList{T}) where T

Remove and return the last item.
"""
function remove_last!(list::DoublyLinkedList{T}) where T
    if isnothing(list.tail)
        throw(ArgumentError("List is empty"))
    end
    result = list.tail.data
    list.tail = list.tail.prev
    if !isnothing(list.tail)
        list.tail.next = nothing
    else
        list.head = nothing
    end
    list.count -= 1
    result
end

"""
    remove_at!(list::DoublyLinkedList{T}, index::Int) where T

Remove and return item at index (1-based).
"""
function remove_at!(list::DoublyLinkedList{T}, index::Int) where T
    if index < 1 || index > list.count
        throw(BoundsError(list, index))
    end
    if index == 1
        return remove_first!(list)
    end
    if index == list.count
        return remove_last!(list)
    end

    if index <= list.count ÷ 2
        current = list.head
        for _ in 1:(index-1)
            current = current.next
        end
    else
        current = list.tail
        for _ in 1:(list.count - index)
            current = current.prev
        end
    end
    result = current.data
    current.prev.next = current.next
    current.next.prev = current.prev
    list.count -= 1
    result
end

"""
    isempty(list::DoublyLinkedList)

Check if the list is empty.
"""
Base.isempty(list::DoublyLinkedList) = list.count == 0

"""
    length(list::DoublyLinkedList)

Return the number of items.
"""
Base.length(list::DoublyLinkedList) = list.count

"""
    size(list::DoublyLinkedList)

Return the number of items.
"""
Base.size(list::DoublyLinkedList) = list.count

"""
    empty!(list::DoublyLinkedList)

Remove all items.
"""
function Base.empty!(list::DoublyLinkedList)
    list.head = nothing
    list.tail = nothing
    list.count = 0
    list
end

"""
    collect(list::DoublyLinkedList{T}) where T

Convert to array.
"""
function Base.collect(list::DoublyLinkedList{T}) where T
    result = T[]
    current = list.head
    while !isnothing(current)
        push!(result, current.data)
        current = current.next
    end
    result
end

# Iterator interface
function Base.iterate(list::DoublyLinkedList, state=list.head)
    isnothing(state) ? nothing : (state.data, state.next)
end
