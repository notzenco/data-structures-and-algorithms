# Doubly Linked List

## Overview

A doubly linked list is a linear data structure where each node contains a pointer to both the **next** and **previous** nodes. This bidirectional linking enables O(1) operations at both ends and efficient deletion when given a node reference.

## Intuition

```
         head                                    tail
          ↓                                        ↓
        ┌───┬───┬───┐   ┌───┬───┬───┐   ┌───┬───┬───┐
 null ← │ ● │ A │ ● │ ↔ │ ● │ B │ ● │ ↔ │ ● │ C │ ● │ → null
        └───┴───┴───┘   └───┴───┴───┘   └───┴───┴───┘
        prev data next  prev data next  prev data next
```

The key advantage: given any node, you can traverse in either direction and remove it in O(1) time.

## How It Works

### Node Structure

```
Node:
    data
    prev  // Pointer to previous node
    next  // Pointer to next node
```

### Core Operations

**Insert at Head**
```
prepend(value):
    new_node = Node(value)
    new_node.next = head
    new_node.prev = null
    if head != null:
        head.prev = new_node
    head = new_node
    if tail == null:
        tail = new_node
```

```
Before: null ← [A] ↔ [B] → null
Insert X at head:
After:  null ← [X] ↔ [A] ↔ [B] → null
```

**Insert at Tail**
```
append(value):
    new_node = Node(value)
    new_node.prev = tail
    new_node.next = null
    if tail != null:
        tail.next = new_node
    tail = new_node
    if head == null:
        head = new_node
```

**Insert After a Node**
```
insert_after(node, value):
    new_node = Node(value)
    new_node.prev = node
    new_node.next = node.next
    if node.next != null:
        node.next.prev = new_node
    else:
        tail = new_node  // Inserting at end
    node.next = new_node
```

```
Insert X after B:
Before: [A] ↔ [B] ↔ [C]
After:  [A] ↔ [B] ↔ [X] ↔ [C]
```

**Delete a Node (given the node)**
```
delete_node(node):
    if node.prev != null:
        node.prev.next = node.next
    else:
        head = node.next  // Deleting head

    if node.next != null:
        node.next.prev = node.prev
    else:
        tail = node.prev  // Deleting tail
```

```
Delete B (given reference to B):
Before: [A] ↔ [B] ↔ [C]
        A.next = C
        C.prev = A
After:  [A] ←──────→ [C]
```

**Traverse Forward**
```
traverse_forward(head):
    current = head
    while current != null:
        process(current.data)
        current = current.next
```

**Traverse Backward**
```
traverse_backward(tail):
    current = tail
    while current != null:
        process(current.data)
        current = current.prev
```

## Mathematical Analysis

### Why O(1) Deletion?

In a singly linked list, to delete node B:
- Must find A (predecessor) to update A.next
- Requires O(n) traversal from head

In a doubly linked list, given node B:
- B.prev gives us A directly
- Update A.next and C.prev
- Constant time: O(1)

### Space Trade-off

Extra pointer per node:
- Singly linked: n nodes × (data + 1 pointer) = n(d + p)
- Doubly linked: n nodes × (data + 2 pointers) = n(d + 2p)

Where p is pointer size (typically 4-8 bytes). For small data, this can nearly double memory usage.

## Time Complexity

| Operation | Time | Notes |
|-----------|------|-------|
| Access by index | O(n) | Can optimize by starting from closer end |
| Search | O(n) | Linear scan |
| Insert at head | O(1) | |
| Insert at tail | O(1) | |
| Insert after node | O(1) | Given the node |
| Insert before node | O(1) | Given the node |
| Delete head | O(1) | |
| Delete tail | O(1) | Unlike singly linked! |
| Delete given node | O(1) | The key advantage |
| Delete by value | O(n) | Must search first |

## Space Complexity

- **Storage**: O(n)
- **Per node overhead**: Two pointers (8-16 bytes typically)
- **Operations**: O(1) extra space

## Use Cases

### Classic Applications

1. **LRU Cache Implementation**
   ```
   Least Recently Used Cache:
   - Doubly linked list for order (most recent at head)
   - Hash map for O(1) lookup
   - Access: Move node to head
   - Insert: Add at head, remove tail if full
   - All operations O(1)
   ```

2. **Browser History**
   ```
   [Page1] ↔ [Page2] ↔ [Page3] ↔ [Current]
                                    ↑
                                  pointer
   Back: move pointer left
   Forward: move pointer right
   Visit: insert after pointer, remove everything after
   ```

3. **Text Editor Buffer**
   - Cursor can move forward/backward
   - Insert/delete at cursor position
   - Undo/redo with bidirectional navigation

4. **Music Player Playlist**
   - Previous/next track
   - Shuffle: O(1) removal and insertion
   - Current track pointer

5. **Deque Implementation**
   - Natural fit: O(1) at both ends
   - No circular array complexity

### Real-World Examples

- **Operating systems**: Process lists, memory management
- **Databases**: B-tree node siblings
- **Undo systems**: More flexible than stack
- **Navigation systems**: Route waypoints

## Trade-offs & Comparisons

### vs. Singly Linked List

| Aspect | Doubly | Singly |
|--------|--------|--------|
| Memory per node | 2 pointers | 1 pointer |
| Delete given node | O(1) | O(n) |
| Delete tail | O(1) | O(n) |
| Backward traversal | O(1) per step | Must restart |
| Implementation | More complex | Simpler |
| Insert before node | O(1) | O(n) |

### vs. Array

| Aspect | Doubly Linked List | Array |
|--------|-------------------|-------|
| Random access | O(n) | O(1) |
| Insert/delete at ends | O(1) | O(1)/O(n) |
| Insert/delete middle | O(1)* | O(n) |
| Memory layout | Scattered | Contiguous |
| Cache performance | Poor | Excellent |

*Given the node reference; O(n) to find the position

### When to Choose Doubly over Singly

- Need to traverse backward
- Need to delete nodes given only the node (not predecessor)
- Need O(1) tail operations
- Implementing LRU cache or similar

## Common Variations

1. **Circular Doubly Linked List**
   - Tail.next = head, head.prev = tail
   - No null pointers
   - Simplifies some edge cases
   ```
        ┌──────────────────────────┐
        ↓                          │
       [A] ↔ [B] ↔ [C] ↔ [D]──────┘
        ↑                    ↓
        └────────────────────┘
   ```

2. **With Sentinel Nodes**
   - Dummy head and tail nodes
   - Eliminates null checks
   - Simplifies insertion/deletion code
   ```
   [DUMMY_HEAD] ↔ [A] ↔ [B] ↔ [C] ↔ [DUMMY_TAIL]
   ```

3. **XOR Linked List**
   - Space optimization: one pointer stores prev XOR next
   - Same functionality with half the pointer overhead
   - More complex implementation

## Common Interview Problems

1. **LRU Cache**: Classic doubly linked list + hash map
2. **Flatten a Multilevel Doubly Linked List**: DFS flattening
3. **Convert Binary Tree to Doubly Linked List**: In-order traversal
4. **Reverse a Doubly Linked List**: Swap prev and next pointers
5. **Design Browser History**: Doubly linked list navigation
6. **Insert in Sorted Doubly Linked List**: Find position and insert

### LRU Cache Implementation Sketch

```
LRUCache:
    capacity
    hash_map: key → node
    list: doubly linked list (head = most recent)

get(key):
    if key not in hash_map:
        return -1
    node = hash_map[key]
    move_to_head(node)
    return node.value

put(key, value):
    if key in hash_map:
        node = hash_map[key]
        node.value = value
        move_to_head(node)
    else:
        node = new Node(key, value)
        hash_map[key] = node
        add_to_head(node)
        if size > capacity:
            tail = remove_tail()
            delete hash_map[tail.key]
```

## Implementation Tips

### Sentinel Pattern

Using dummy head and tail eliminates edge cases:

```
class DoublyLinkedList:
    def __init__(self):
        self.head = Node(None)  # Dummy
        self.tail = Node(None)  # Dummy
        self.head.next = self.tail
        self.tail.prev = self.head

    def insert_after(self, node, value):
        # No null checks needed!
        new_node = Node(value)
        new_node.prev = node
        new_node.next = node.next
        node.next.prev = new_node
        node.next = new_node
```

### Common Pitfalls

1. **Forgetting to update both pointers**: Always update both prev and next
2. **Null pointer exceptions**: Handle empty list cases
3. **Head/tail maintenance**: Update when inserting/deleting at ends
4. **Memory leaks**: Clear both pointers when removing

## Further Reading

- CLRS Chapter 10.2 (Linked Lists)
- [Doubly Linked List - Wikipedia](https://en.wikipedia.org/wiki/Doubly_linked_list)
- [Visualgo - Linked List](https://visualgo.net/en/list)
- [LRU Cache - LeetCode](https://leetcode.com/problems/lru-cache/)
