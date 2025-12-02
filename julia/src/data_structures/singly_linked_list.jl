"""
Singly Linked List - Forward-only linked list

Time Complexity:
- prepend!: O(1)
- append!: O(1) with tail pointer
- get: O(n)
- remove_at!: O(n)
"""

mutable struct SLLNode{T}
    data::T
    next::Union{SLLNode{T}, Nothing}
end

mutable struct SinglyLinkedList{T}
    head::Union{SLLNode{T}, Nothing}
    tail::Union{SLLNode{T}, Nothing}
    count::Int

    SinglyLinkedList{T}() where T = new{T}(nothing, nothing, 0)
end

SinglyLinkedList() = SinglyLinkedList{Any}()

"""
    prepend!(list::SinglyLinkedList{T}, item::T) where T

Add an item to the front.
"""
function prepend!(list::SinglyLinkedList{T}, item::T) where T
    node = SLLNode{T}(item, list.head)
    list.head = node
    if isnothing(list.tail)
        list.tail = node
    end
    list.count += 1
    list
end

"""
    append!(list::SinglyLinkedList{T}, item::T) where T

Add an item to the back.
"""
function Base.append!(list::SinglyLinkedList{T}, item::T) where T
    node = SLLNode{T}(item, nothing)
    if isnothing(list.tail)
        list.head = node
        list.tail = node
    else
        list.tail.next = node
        list.tail = node
    end
    list.count += 1
    list
end

"""
    get(list::SinglyLinkedList{T}, index::Int) where T

Get item at index (1-based).
"""
function get(list::SinglyLinkedList{T}, index::Int) where T
    if index < 1 || index > list.count
        throw(BoundsError(list, index))
    end
    current = list.head
    for _ in 1:(index-1)
        current = current.next
    end
    current.data
end

Base.getindex(list::SinglyLinkedList, i::Int) = get(list, i)

"""
    set!(list::SinglyLinkedList{T}, index::Int, item::T) where T

Set item at index (1-based).
"""
function set!(list::SinglyLinkedList{T}, index::Int, item::T) where T
    if index < 1 || index > list.count
        throw(BoundsError(list, index))
    end
    current = list.head
    for _ in 1:(index-1)
        current = current.next
    end
    current.data = item
    list
end

"""
    remove_first!(list::SinglyLinkedList{T}) where T

Remove and return the first item.
"""
function remove_first!(list::SinglyLinkedList{T}) where T
    if isnothing(list.head)
        throw(ArgumentError("List is empty"))
    end
    result = list.head.data
    list.head = list.head.next
    if isnothing(list.head)
        list.tail = nothing
    end
    list.count -= 1
    result
end

"""
    remove_at!(list::SinglyLinkedList{T}, index::Int) where T

Remove and return item at index (1-based).
"""
function remove_at!(list::SinglyLinkedList{T}, index::Int) where T
    if index < 1 || index > list.count
        throw(BoundsError(list, index))
    end
    if index == 1
        return remove_first!(list)
    end
    current = list.head
    for _ in 1:(index-2)
        current = current.next
    end
    result = current.next.data
    current.next = current.next.next
    if isnothing(current.next)
        list.tail = current
    end
    list.count -= 1
    result
end

"""
    find(list::SinglyLinkedList{T}, item::T) where T

Find index of item, returns nothing if not found.
"""
function Base.findfirst(list::SinglyLinkedList{T}, item::T) where T
    current = list.head
    index = 1
    while !isnothing(current)
        if current.data == item
            return index
        end
        current = current.next
        index += 1
    end
    nothing
end

"""
    contains(list::SinglyLinkedList{T}, item::T) where T

Check if list contains item.
"""
contains(list::SinglyLinkedList{T}, item::T) where T = !isnothing(findfirst(list, item))

"""
    isempty(list::SinglyLinkedList)

Check if the list is empty.
"""
Base.isempty(list::SinglyLinkedList) = list.count == 0

"""
    length(list::SinglyLinkedList)

Return the number of items.
"""
Base.length(list::SinglyLinkedList) = list.count

"""
    size(list::SinglyLinkedList)

Return the number of items.
"""
Base.size(list::SinglyLinkedList) = list.count

"""
    empty!(list::SinglyLinkedList)

Remove all items.
"""
function Base.empty!(list::SinglyLinkedList)
    list.head = nothing
    list.tail = nothing
    list.count = 0
    list
end

"""
    collect(list::SinglyLinkedList{T}) where T

Convert to array.
"""
function Base.collect(list::SinglyLinkedList{T}) where T
    result = T[]
    current = list.head
    while !isnothing(current)
        push!(result, current.data)
        current = current.next
    end
    result
end

# Iterator interface
function Base.iterate(list::SinglyLinkedList, state=list.head)
    isnothing(state) ? nothing : (state.data, state.next)
end
