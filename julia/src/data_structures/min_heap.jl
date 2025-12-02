"""
Min Heap - Binary min heap / priority queue

Time Complexity:
- insert!: O(log n)
- extract_min!: O(log n)
- peek: O(1)
"""

mutable struct MinHeap{T}
    items::Vector{T}

    MinHeap{T}() where T = new{T}(T[])
end

MinHeap() = MinHeap{Any}()

_parent(i::Int) = (i - 1) ÷ 2 + 1
_left_child(i::Int) = 2 * i
_right_child(i::Int) = 2 * i + 1

function _sift_up!(heap::MinHeap{T}, index::Int) where T
    while index > 1 && heap.items[_parent(index)] > heap.items[index]
        parent_idx = _parent(index)
        heap.items[parent_idx], heap.items[index] = heap.items[index], heap.items[parent_idx]
        index = parent_idx
    end
end

function _sift_down!(heap::MinHeap{T}, index::Int) where T
    n = length(heap.items)
    while true
        smallest = index
        left = _left_child(index)
        right = _right_child(index)

        if left <= n && heap.items[left] < heap.items[smallest]
            smallest = left
        end
        if right <= n && heap.items[right] < heap.items[smallest]
            smallest = right
        end

        if smallest == index
            break
        end

        heap.items[index], heap.items[smallest] = heap.items[smallest], heap.items[index]
        index = smallest
    end
end

"""
    insert!(heap::MinHeap{T}, item::T) where T

Insert an item into the heap.
"""
function Base.insert!(heap::MinHeap{T}, item::T) where T
    push!(heap.items, item)
    _sift_up!(heap, length(heap.items))
    heap
end

"""
    push!(heap::MinHeap{T}, item::T) where T

Insert an item into the heap (alias for insert!).
"""
function Base.push!(heap::MinHeap{T}, item::T) where T
    insert!(heap, item)
end

"""
    extract_min!(heap::MinHeap{T}) where T

Remove and return the minimum item.
Throws ArgumentError if heap is empty.
"""
function extract_min!(heap::MinHeap{T}) where T
    if isempty(heap)
        throw(ArgumentError("Heap is empty"))
    end
    result = heap.items[1]
    heap.items[1] = heap.items[end]
    pop!(heap.items)
    if !isempty(heap.items)
        _sift_down!(heap, 1)
    end
    result
end

"""
    pop!(heap::MinHeap{T}) where T

Remove and return the minimum item (alias for extract_min!).
"""
Base.pop!(heap::MinHeap) = extract_min!(heap)

"""
    peek(heap::MinHeap{T}) where T

Return the minimum item without removing it.
Throws ArgumentError if heap is empty.
"""
function peek(heap::MinHeap{T}) where T
    if isempty(heap)
        throw(ArgumentError("Heap is empty"))
    end
    heap.items[1]
end

"""
    isempty(heap::MinHeap)

Check if the heap is empty.
"""
Base.isempty(heap::MinHeap) = isempty(heap.items)

"""
    length(heap::MinHeap)

Return the number of items.
"""
Base.length(heap::MinHeap) = length(heap.items)

"""
    size(heap::MinHeap)

Return the number of items.
"""
Base.size(heap::MinHeap) = length(heap.items)

"""
    empty!(heap::MinHeap)

Remove all items.
"""
function Base.empty!(heap::MinHeap)
    empty!(heap.items)
    heap
end

"""
    heapify(items::Vector{T}) where T

Create a min heap from a vector.
"""
function heapify(items::Vector{T}) where T
    heap = MinHeap{T}()
    heap.items = copy(items)
    n = length(heap.items)
    for i in (n ÷ 2):-1:1
        _sift_down!(heap, i)
    end
    heap
end

"""
    collect(heap::MinHeap)

Convert to array (internal order, not sorted).
"""
Base.collect(heap::MinHeap) = copy(heap.items)

"""
    decrease_key!(heap::MinHeap{T}, index::Int, new_value::T) where T

Decrease the value at index (must be smaller than current).
"""
function decrease_key!(heap::MinHeap{T}, index::Int, new_value::T) where T
    if index < 1 || index > length(heap.items)
        throw(BoundsError(heap.items, index))
    end
    if new_value >= heap.items[index]
        throw(ArgumentError("New value must be smaller"))
    end
    heap.items[index] = new_value
    _sift_up!(heap, index)
    heap
end
