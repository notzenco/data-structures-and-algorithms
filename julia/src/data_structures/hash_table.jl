"""
Hash Table - Open addressing with linear probing

Time Complexity (average):
- put!: O(1)
- get: O(1)
- remove!: O(1)
- contains: O(1)
"""

const EMPTY = 0
const OCCUPIED = 1
const DELETED = 2

mutable struct HTEntry{K,V}
    key::K
    value::V
    state::Int

    HTEntry{K,V}() where {K,V} = new{K,V}()
    HTEntry{K,V}(k::K, v::V, s::Int) where {K,V} = new{K,V}(k, v, s)
end

mutable struct HashTable{K,V}
    buckets::Vector{HTEntry{K,V}}
    count::Int
    capacity::Int

    function HashTable{K,V}(capacity::Int=16) where {K,V}
        buckets = [HTEntry{K,V}() for _ in 1:capacity]
        for b in buckets
            b.state = EMPTY
        end
        new{K,V}(buckets, 0, capacity)
    end
end

HashTable() = HashTable{Any,Any}()

function _hash_index(ht::HashTable{K,V}, key::K) where {K,V}
    (hash(key) % ht.capacity) + 1
end

function _find_slot(ht::HashTable{K,V}, key::K) where {K,V}
    idx = _hash_index(ht, key)
    first_deleted = 0

    for _ in 1:ht.capacity
        entry = ht.buckets[idx]
        if entry.state == EMPTY
            return first_deleted != 0 ? first_deleted : idx
        elseif entry.state == DELETED
            if first_deleted == 0
                first_deleted = idx
            end
        elseif entry.key == key
            return idx
        end
        idx = (idx % ht.capacity) + 1
    end

    first_deleted != 0 ? first_deleted : 0
end

function _resize!(ht::HashTable{K,V}) where {K,V}
    old_buckets = ht.buckets
    ht.capacity *= 2
    ht.buckets = [HTEntry{K,V}() for _ in 1:ht.capacity]
    for b in ht.buckets
        b.state = EMPTY
    end
    ht.count = 0

    for entry in old_buckets
        if entry.state == OCCUPIED
            put!(ht, entry.key, entry.value)
        end
    end
end

"""
    put!(ht::HashTable{K,V}, key::K, value::V) where {K,V}

Insert or update a key-value pair.
"""
function put!(ht::HashTable{K,V}, key::K, value::V) where {K,V}
    if (ht.count + 1) / ht.capacity > 0.75
        _resize!(ht)
    end

    idx = _find_slot(ht, key)
    if idx == 0
        _resize!(ht)
        idx = _find_slot(ht, key)
    end

    if ht.buckets[idx].state != OCCUPIED
        ht.count += 1
    end
    ht.buckets[idx] = HTEntry{K,V}(key, value, OCCUPIED)
    ht
end

Base.setindex!(ht::HashTable{K,V}, v::V, k::K) where {K,V} = put!(ht, k, v)

"""
    get(ht::HashTable{K,V}, key::K) where {K,V}

Get value by key. Throws KeyError if not found.
"""
function get(ht::HashTable{K,V}, key::K) where {K,V}
    idx = _hash_index(ht, key)

    for _ in 1:ht.capacity
        entry = ht.buckets[idx]
        if entry.state == EMPTY
            throw(KeyError(key))
        elseif entry.state == OCCUPIED && entry.key == key
            return entry.value
        end
        idx = (idx % ht.capacity) + 1
    end

    throw(KeyError(key))
end

Base.getindex(ht::HashTable, key) = get(ht, key)

"""
    get(ht::HashTable{K,V}, key::K, default::V) where {K,V}

Get value by key, or return default if not found.
"""
function get(ht::HashTable{K,V}, key::K, default::V) where {K,V}
    try
        return get(ht, key)
    catch e
        if isa(e, KeyError)
            return default
        end
        rethrow(e)
    end
end

"""
    contains(ht::HashTable{K,V}, key::K) where {K,V}

Check if key exists.
"""
function contains(ht::HashTable{K,V}, key::K) where {K,V}
    idx = _hash_index(ht, key)

    for _ in 1:ht.capacity
        entry = ht.buckets[idx]
        if entry.state == EMPTY
            return false
        elseif entry.state == OCCUPIED && entry.key == key
            return true
        end
        idx = (idx % ht.capacity) + 1
    end

    false
end

Base.haskey(ht::HashTable, key) = contains(ht, key)

"""
    remove!(ht::HashTable{K,V}, key::K) where {K,V}

Remove a key-value pair. Returns true if key was found.
"""
function remove!(ht::HashTable{K,V}, key::K) where {K,V}
    idx = _hash_index(ht, key)

    for _ in 1:ht.capacity
        entry = ht.buckets[idx]
        if entry.state == EMPTY
            return false
        elseif entry.state == OCCUPIED && entry.key == key
            ht.buckets[idx].state = DELETED
            ht.count -= 1
            return true
        end
        idx = (idx % ht.capacity) + 1
    end

    false
end

Base.delete!(ht::HashTable, key) = remove!(ht, key)

"""
    isempty(ht::HashTable)

Check if the hash table is empty.
"""
Base.isempty(ht::HashTable) = ht.count == 0

"""
    length(ht::HashTable)

Return the number of entries.
"""
Base.length(ht::HashTable) = ht.count

"""
    size(ht::HashTable)

Return the number of entries.
"""
Base.size(ht::HashTable) = ht.count

"""
    empty!(ht::HashTable)

Remove all entries.
"""
function Base.empty!(ht::HashTable{K,V}) where {K,V}
    for i in 1:ht.capacity
        ht.buckets[i].state = EMPTY
    end
    ht.count = 0
    ht
end

"""
    keys(ht::HashTable{K,V}) where {K,V}

Get all keys.
"""
function Base.keys(ht::HashTable{K,V}) where {K,V}
    result = K[]
    for entry in ht.buckets
        if entry.state == OCCUPIED
            push!(result, entry.key)
        end
    end
    result
end

"""
    values(ht::HashTable{K,V}) where {K,V}

Get all values.
"""
function Base.values(ht::HashTable{K,V}) where {K,V}
    result = V[]
    for entry in ht.buckets
        if entry.state == OCCUPIED
            push!(result, entry.value)
        end
    end
    result
end

# Iterator interface
function Base.iterate(ht::HashTable, state=1)
    while state <= ht.capacity
        if ht.buckets[state].state == OCCUPIED
            entry = ht.buckets[state]
            return ((entry.key, entry.value), state + 1)
        end
        state += 1
    end
    nothing
end
