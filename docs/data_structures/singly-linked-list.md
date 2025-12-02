# Singly Linked List

## Overview

A singly linked list is a linear data structure where elements (nodes) are stored in non-contiguous memory locations. Each node contains data and a pointer to the next node, forming a chain. The list is traversed in one direction: from head to tail.

## Intuition

```
 head
  ↓
┌───┬───┐    ┌───┬───┐    ┌───┬───┐    ┌───┬───┐
│ A │ ●─┼───>│ B │ ●─┼───>│ C │ ●─┼───>│ D │ ╳ │
└───┴───┘    └───┴───┘    └───┴───┘    └───┴───┘
 data next    data next    data next    data next
                                              ↑
                                            null
```

Unlike arrays where elements sit next to each other in memory, linked list nodes can be anywhere. The pointers create the logical sequence.

## How It Works

### Node Structure

```
Node:
    data    // The stored value
    next    // Pointer to next node (or null)
```

### Core Operations

**Traversal**
```
traverse(head):
    current = head
    while current != null:
        process(current.data)
        current = current.next
```

**Search**
```
search(head, target):
    current = head
    while current != null:
        if current.data == target:
            return current
        current = current.next
    return null  // Not found
```

**Insert at Head (Prepend)**
```
prepend(head, value):
    new_node = Node(value)
    new_node.next = head
    return new_node  // New head
```

```
Before: head → [B] → [C] → null
After prepend(A):
        head → [A] → [B] → [C] → null
```

**Insert at Tail (Append)**
```
append(head, value):
    new_node = Node(value)
    if head == null:
        return new_node
    current = head
    while current.next != null:
        current = current.next
    current.next = new_node
    return head
```

**Insert After a Node**
```
insert_after(node, value):
    new_node = Node(value)
    new_node.next = node.next
    node.next = new_node
```

```
Before: [A] → [B] → [C]
Insert X after B:
        [A] → [B] → [X] → [C]
             ↓    ↑
         node.next = new_node
              new_node.next = C
```

**Delete a Node**
```
delete(head, target):
    if head == null:
        return null
    if head.data == target:
        return head.next  // New head
    current = head
    while current.next != null:
        if current.next.data == target:
            current.next = current.next.next
            return head
        current = current.next
    return head  // Not found
```

```
Delete B:
Before: [A] → [B] → [C]
             ↑
         current.next

After:  [A] ──────→ [C]
        current.next = current.next.next
```

## Mathematical Analysis

### Time Complexity Derivation

**Access by Index**: Must traverse from head
- Best case: Index 0, O(1)
- Worst case: Index n-1, visit all nodes, O(n)
- Average: n/2 nodes visited, O(n)

**Search**: Linear scan required
- Best case: Target at head, O(1)
- Worst case: Target at tail or missing, O(n)
- Average: O(n)

**Insert at Head**: Just pointer manipulation
- Always O(1): create node, update two pointers

**Insert at Tail** (without tail pointer):
- Must traverse to end: O(n)
- With tail pointer: O(1)

**Delete**: Must find predecessor
- O(n) to find, O(1) to remove

## Time Complexity

| Operation | Time | Notes |
|-----------|------|-------|
| Access by index | O(n) | Must traverse |
| Search | O(n) | Linear scan |
| Insert at head | O(1) | |
| Insert at tail | O(n) | O(1) with tail pointer |
| Insert after node | O(1) | Given the node |
| Delete head | O(1) | |
| Delete by value | O(n) | Need to find predecessor |
| Delete after node | O(1) | Given the predecessor |

## Space Complexity

- **Storage**: O(n) for n nodes
- **Per node overhead**: One pointer (typically 4-8 bytes)
- **Operations**: O(1) extra space for most operations

## Use Cases

### When to Use Singly Linked Lists

1. **Frequent insertions/deletions at the beginning**
   - O(1) prepend vs. O(n) for arrays

2. **Unknown or highly variable size**
   - No need to pre-allocate or resize

3. **Memory-constrained environments**
   - Less overhead than doubly linked (one pointer vs. two)

4. **Implementing other data structures**
   - Stack (push/pop at head)
   - Queue (with tail pointer)
   - Hash table chaining

### Real-World Examples

- **Undo functionality**: Simple chain of states
- **Music playlist**: Next track pointer
- **Image viewer**: Next image pointer
- **Symbol tables**: Chaining in hash tables
- **Memory allocation**: Free list management

### When NOT to Use

- **Need random access**: Use array
- **Frequent backward traversal**: Use doubly linked list
- **Cache-sensitive applications**: Arrays have better locality
- **Need to delete arbitrary nodes quickly**: Doubly linked better

## Trade-offs & Comparisons

### vs. Array/Dynamic Array

| Aspect | Singly Linked List | Array |
|--------|-------------------|-------|
| Random access | O(n) | O(1) |
| Insert at front | O(1) | O(n) |
| Insert at end | O(n)* | O(1)** |
| Insert in middle | O(1)† | O(n) |
| Memory layout | Scattered | Contiguous |
| Cache performance | Poor | Excellent |
| Memory overhead | One pointer/node | None |
| Size flexibility | Dynamic | Fixed (or resize) |

*O(1) with tail pointer
**Amortized for dynamic array
†O(1) for the insertion itself, but O(n) to find the position

### vs. Doubly Linked List

| Aspect | Singly | Doubly |
|--------|--------|--------|
| Memory per node | Less (1 pointer) | More (2 pointers) |
| Backward traversal | O(n) from start | O(1) from any node |
| Delete given node | Need predecessor | O(1) |
| Implementation | Simpler | More complex |

## Common Variations

1. **With Tail Pointer**
   - O(1) append operations
   - Must maintain tail on deletions

2. **Circular Linked List**
   - Last node points to head
   - No null pointers
   - Good for round-robin scheduling

3. **Sorted Linked List**
   - Maintains order on insertion
   - O(n) insert, O(n) search (but often faster in practice)

4. **Skip List**
   - Multiple layers of linked lists
   - O(log n) search, insert, delete
   - Alternative to balanced trees

## Common Interview Problems

1. **Reverse a Linked List**: Iterative and recursive
2. **Detect Cycle**: Floyd's tortoise and hare
3. **Find Middle Node**: Two-pointer technique
4. **Merge Two Sorted Lists**: Classic merge operation
5. **Remove Nth Node from End**: Two-pointer technique
6. **Palindrome Check**: Reverse second half and compare
7. **Intersection of Two Lists**: Find meeting point

### Classic Technique: Two Pointers

**Find Middle Node:**
```
slow = head
fast = head
while fast != null and fast.next != null:
    slow = slow.next
    fast = fast.next.next
return slow  // Middle node
```

**Detect Cycle (Floyd's Algorithm):**
```
slow = fast = head
while fast != null and fast.next != null:
    slow = slow.next
    fast = fast.next.next
    if slow == fast:
        return true  // Cycle detected
return false
```

## Implementation Tips

### Sentinel/Dummy Node

Using a dummy head simplifies edge cases:
```
dummy = Node(0)
dummy.next = head
// Now operations don't need special head handling
```

### Common Pitfalls

1. **Losing reference to head**: Always keep head pointer
2. **Null pointer exceptions**: Check before accessing .next
3. **Memory leaks**: In languages without GC, free deleted nodes
4. **Off-by-one**: Careful with "insert after" vs "insert before"

## Further Reading

- CLRS Chapter 10.2 (Linked Lists)
- [Linked List - Wikipedia](https://en.wikipedia.org/wiki/Linked_list)
- [Visualgo - Linked List](https://visualgo.net/en/list)
