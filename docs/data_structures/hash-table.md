# Hash Table (Open Addressing)

## Overview

A hash table is a data structure that implements an associative array (dictionary), mapping keys to values. It uses a **hash function** to compute an index into an array of buckets, from which the desired value can be found. Open addressing resolves collisions by probing for alternative slots within the array itself.

## Intuition

```
Key "apple" → hash("apple") = 42 → index = 42 % 10 = 2

Index:  0     1     2        3     4     5     6     7     8     9
      [   ] [   ] [apple:5] [   ] [   ] [   ] [   ] [   ] [   ] [   ]

Collision: "banana" also hashes to index 2
→ Linear probing: try 3, then 4, then 5...
→ Place at first empty slot

Index:  0     1     2        3         4     5     6     7     8     9
      [   ] [   ] [apple:5] [banana:3] [   ] [   ] [   ] [   ] [   ] [   ]
```

The magic: converting any key into an array index for O(1) average access.

## How It Works

### Hash Function

A good hash function should:
1. Be deterministic (same input → same output)
2. Distribute keys uniformly across the array
3. Be fast to compute

**Example: Simple string hash**
```
hash(string):
    h = 0
    for char in string:
        h = 31 * h + char_code(char)
    return h
```

### Open Addressing Probing Strategies

**Linear Probing**
```
probe(key, i) = (hash(key) + i) % capacity

Sequence: h, h+1, h+2, h+3, ...
```
- Simple but causes "primary clustering"
- Clusters of occupied slots grow

**Quadratic Probing**
```
probe(key, i) = (hash(key) + c1*i + c2*i²) % capacity

Sequence: h, h+1, h+3, h+6, h+10, ...
```
- Reduces primary clustering
- May not visit all slots (capacity must be prime or power of 2)

**Double Hashing**
```
probe(key, i) = (hash1(key) + i * hash2(key)) % capacity

hash2 must never return 0
```
- Best distribution
- Requires two hash functions

### Core Operations

**Insert**
```
insert(key, value):
    if load_factor > threshold:
        resize(capacity * 2)

    i = 0
    while true:
        index = probe(key, i)
        if table[index] is EMPTY or DELETED:
            table[index] = (key, value)
            size++
            return
        if table[index].key == key:
            table[index].value = value  // Update
            return
        i++
```

**Search**
```
search(key):
    i = 0
    while true:
        index = probe(key, i)
        if table[index] is EMPTY:
            return NOT_FOUND
        if table[index] is not DELETED and table[index].key == key:
            return table[index].value
        i++
        if i == capacity:
            return NOT_FOUND
```

**Delete**
```
delete(key):
    i = 0
    while true:
        index = probe(key, i)
        if table[index] is EMPTY:
            return  // Not found
        if table[index].key == key:
            table[index] = DELETED  // Tombstone marker
            size--
            return
        i++
```

### Why Tombstones?

```
Insert A at index 5
Insert B at index 5 → collision → goes to 6
Insert C at index 5 → collision → goes to 7

[5:A] [6:B] [7:C]

Delete A:
[5:EMPTY] [6:B] [7:C]

Search for B:
- hash(B) = 5
- Index 5 is EMPTY → STOP → "B not found" ← WRONG!

With tombstone:
[5:DELETED] [6:B] [7:C]

Search for B:
- hash(B) = 5
- Index 5 is DELETED → continue probing
- Index 6 has B → FOUND ✓
```

## Mathematical Analysis

### Average Case Analysis

Assuming uniform hashing and load factor α = n/m (items/capacity):

**Unsuccessful Search (Linear Probing)**
Expected probes ≈ ½(1 + 1/(1-α)²)

**Successful Search (Linear Probing)**
Expected probes ≈ ½(1 + 1/(1-α))

| Load Factor α | Unsuccessful | Successful |
|---------------|--------------|------------|
| 0.5           | 2.5          | 1.5        |
| 0.75          | 8.5          | 2.5        |
| 0.9           | 50.5         | 5.5        |

**Key insight**: Performance degrades rapidly above 70-75% load factor.

### Why Resize at ~75%?

At α = 0.75:
- Average 2.5 probes for successful search (acceptable)
- Average 8.5 probes for unsuccessful search (still acceptable)

At α = 0.9:
- Performance becomes unacceptable
- Nearly linear-time operations

### Amortized Resize Cost

When resizing from m to 2m:
- Copy n elements
- Each requires O(1) expected insertion
- Total: O(n)

Amortized over n/2 insertions since last resize: O(1) per insertion

## Time Complexity

| Operation | Average | Worst Case |
|-----------|---------|------------|
| Insert    | O(1)    | O(n)       |
| Search    | O(1)    | O(n)       |
| Delete    | O(1)    | O(n)       |

Worst case occurs when:
- All keys hash to the same index (pathological hash function)
- Table is nearly full

## Space Complexity

- **Storage**: O(n) for n key-value pairs
- **Overhead**: Typically 25-100% extra capacity to maintain low load factor
- **Per entry**: Key + value + possible metadata

## Use Cases

### When to Use Hash Tables

1. **Need O(1) key-value lookup**
   - Caches
   - Symbol tables in compilers
   - Database indexing

2. **Counting/frequency problems**
   ```
   count = {}
   for item in data:
       count[item] = count.get(item, 0) + 1
   ```

3. **Deduplication**
   ```
   seen = set()  // Hash set
   for item in data:
       if item in seen:
           // duplicate
       seen.add(item)
   ```

4. **Two-sum type problems**
   ```
   // Find pair summing to target
   seen = {}
   for num in array:
       complement = target - num
       if complement in seen:
           return (seen[complement], current_index)
       seen[num] = current_index
   ```

### Real-World Examples

- **Python dict, JavaScript Object/Map**: Core language feature
- **Database indexes**: Fast record lookup
- **Caching systems**: Memcached, Redis
- **DNS resolution**: Domain → IP mapping
- **Compilers**: Symbol tables

### When NOT to Use

- Need ordered iteration → Use TreeMap/BST
- Need range queries → Use BST
- Keys aren't hashable → Use other structure
- Memory is very limited → Consider alternatives

## Trade-offs & Comparisons

### Open Addressing vs. Chaining

| Aspect | Open Addressing | Chaining |
|--------|-----------------|----------|
| Memory layout | Better cache locality | Pointer overhead |
| Memory usage | No extra pointers | Extra for linked lists |
| Load factor | Must stay < 1 | Can exceed 1 |
| Deletion | Needs tombstones | Simple removal |
| Implementation | More complex | Simpler |
| Worst case | All in one cluster | All in one bucket |

### Hash Table vs. BST

| Aspect | Hash Table | Balanced BST |
|--------|------------|--------------|
| Average lookup | O(1) | O(log n) |
| Worst case | O(n) | O(log n) |
| Ordered iteration | No | Yes |
| Range queries | No | Yes |
| Memory overhead | ~1.5x | 2 pointers/node |
| Implementation | Moderate | Complex |

## Common Variations

1. **Robin Hood Hashing**
   - Rich entries (far from home) give slots to poor entries
   - Better worst-case performance
   - Used in Rust's HashMap

2. **Cuckoo Hashing**
   - Two hash functions, two tables
   - O(1) worst-case lookup
   - Insertions may require rehashing

3. **Hopscotch Hashing**
   - Entries stay within H slots of home
   - Good cache performance

4. **Swiss Table (Google)**
   - SIMD-accelerated probing
   - Used in Abseil, Rust hashbrown

## Implementation Considerations

### Choosing Table Size

- **Prime numbers**: Better distribution for some hash functions
- **Powers of 2**: Fast modulo via bitmask (index = hash & (size-1))
- Modern implementations often use powers of 2 with good hash functions

### Load Factor Thresholds

| Implementation | Max Load Factor |
|----------------|-----------------|
| Java HashMap   | 0.75            |
| Python dict    | 0.66            |
| Go map         | 0.65            |
| Rust HashMap   | 0.875 (Robin Hood) |

### Handling Hash Collisions

Good hash functions minimize collisions, but attacks can force collisions:
- **Hash flooding attack**: Malicious keys all hash to same index
- **Mitigation**: Random seed in hash function, SipHash

## Common Interview Problems

1. **Two Sum**: Use hash map for complement lookup
2. **Group Anagrams**: Hash sorted characters
3. **LRU Cache**: Hash map + doubly linked list
4. **First Non-Repeating Character**: Count frequencies
5. **Subarray Sum Equals K**: Prefix sum + hash map
6. **Longest Consecutive Sequence**: Hash set membership

## Further Reading

- CLRS Chapter 11 (Hash Tables)
- [Hash Table - Wikipedia](https://en.wikipedia.org/wiki/Hash_table)
- [Open Addressing - Wikipedia](https://en.wikipedia.org/wiki/Open_addressing)
- [Visualgo - Hash Table](https://visualgo.net/en/hashtable)
- [Robin Hood Hashing](https://programming.guide/robin-hood-hashing.html)
