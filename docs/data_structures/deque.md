# Deque (Double-Ended Queue)

## Overview

A deque (pronounced "deck") is a linear data structure that allows insertion and deletion at **both ends**. It combines the capabilities of both stacks and queues, providing O(1) operations at the front and back.

## Intuition

```
    ← push_front    pop_front →
         ┌───┬───┬───┬───┐
         │ 1 │ 2 │ 3 │ 4 │
         └───┴───┴───┴───┘
    ← pop_back    push_back →
```

A deque is like a line of people where:
- People can join at either end
- People can leave from either end
- You can see who's at the front or back

## How It Works

### Core Operations

```
push_front(element):  Add to front
push_back(element):   Add to rear
pop_front():          Remove from front
pop_back():           Remove from rear
front():              View front element
back():               View rear element
isEmpty():            Check if empty
```

### Circular Array Implementation

```
Deque:
    array[capacity]
    front = 0
    rear = 0
    size = 0

push_back(element):
    if size == capacity: resize()
    array[rear] = element
    rear = (rear + 1) % capacity
    size++

push_front(element):
    if size == capacity: resize()
    front = (front - 1 + capacity) % capacity
    array[front] = element
    size++

pop_back():
    rear = (rear - 1 + capacity) % capacity
    element = array[rear]
    size--
    return element

pop_front():
    element = array[front]
    front = (front + 1) % capacity
    size--
    return element
```

**Visualization of Circular Deque:**
```
capacity = 6, initially empty:
    [_][_][_][_][_][_]
     ↑
   front,rear

push_back(A), push_back(B):
    [A][B][_][_][_][_]
     ↑     ↑
   front  rear

push_front(Z):
    [A][B][_][_][_][Z]
                   ↑  ↑
                front rear (wrapped)

State: Z, A, B (front to back)
```

### Doubly Linked List Implementation

```
Node:
    data
    prev
    next

Deque:
    head = null
    tail = null
    size = 0

push_front(element):
    node = Node(element)
    node.next = head
    if head != null:
        head.prev = node
    head = node
    if tail == null:
        tail = node
    size++

push_back(element):
    node = Node(element)
    node.prev = tail
    if tail != null:
        tail.next = node
    tail = node
    if head == null:
        head = node
    size++

pop_front():
    element = head.data
    head = head.next
    if head != null:
        head.prev = null
    else:
        tail = null
    size--
    return element

pop_back():
    element = tail.data
    tail = tail.prev
    if tail != null:
        tail.next = null
    else:
        head = null
    size--
    return element
```

## Mathematical Analysis

### Modular Arithmetic for Front Operations

Moving front backward requires handling negative indices:
```
front = (front - 1 + capacity) % capacity
```

Why `+ capacity`?
- If front = 0 and we subtract 1, we get -1
- (-1 + 6) % 6 = 5, correctly wrapping to the end

### Amortized Analysis (Dynamic Array Version)

Same as dynamic array analysis:
- Doubling capacity when full
- Total cost for n operations = O(n)
- Amortized cost per operation = O(1)

## Time Complexity

| Operation    | Circular Array | Doubly Linked List |
|--------------|----------------|-------------------|
| push_front() | O(1)*          | O(1)              |
| push_back()  | O(1)*          | O(1)              |
| pop_front()  | O(1)           | O(1)              |
| pop_back()   | O(1)           | O(1)              |
| front()      | O(1)           | O(1)              |
| back()       | O(1)           | O(1)              |
| isEmpty()    | O(1)           | O(1)              |
| Random access| O(1)           | O(n)              |

*O(1) amortized with dynamic resizing

## Space Complexity

- **Circular array**: O(n), with potential unused capacity
- **Doubly linked list**: O(n), with two pointers per node

## Use Cases

### Classic Applications

1. **Sliding Window Maximum/Minimum**
   ```
   Array: [1, 3, -1, -3, 5, 3, 6, 7], window size k=3

   Use monotonic deque to track max in current window:
   - Push indices to back
   - Pop from back while current > deque.back()
   - Pop from front when index out of window
   - Front always has max for current window

   Result: [3, 3, 5, 5, 6, 7]
   ```

2. **Palindrome Checking**
   ```
   Load characters into deque
   While size > 1:
       if pop_front() != pop_back():
           return false
   return true
   ```

3. **Work Stealing Scheduler**
   - Each thread has a deque of tasks
   - Owner pushes/pops from one end (like a stack)
   - Thieves steal from the other end (like a queue)

4. **Browser History with Forward**
   - More flexible than stack-based implementation
   - Can limit history size by removing from back

5. **Undo/Redo with Limited History**
   - Push operations to back
   - Pop from back for undo
   - When limit reached, pop oldest from front

### Real-World Examples

- **Python's `collections.deque`**: Thread-safe, efficient, widely used
- **C++ `std::deque`**: Standard container with O(1) access
- **Task queues**: Work stealing in parallel computing
- **A-Steal algorithm**: Load balancing in multiprocessor systems

## Trade-offs & Comparisons

### Implementation Comparison

| Aspect | Circular Array | Doubly Linked List |
|--------|----------------|-------------------|
| Random access | O(1) | O(n) |
| Memory layout | Contiguous | Scattered |
| Cache performance | Better | Worse |
| Memory overhead | Unused capacity | Two pointers/node |
| Max size | Can grow | Unlimited |

### Deque vs. Other Data Structures

| Need | Best Choice |
|------|-------------|
| LIFO only | Stack |
| FIFO only | Queue |
| Both ends | Deque |
| Random access + both ends | Deque (array-based) |
| Frequent middle insertions | Linked List |

### Python's deque vs. list

| Operation | list | deque |
|-----------|------|-------|
| append (right) | O(1)* | O(1)* |
| pop (right) | O(1) | O(1) |
| insert (left) | O(n) | O(1) |
| pop (left) | O(n) | O(1) |
| Random access | O(1) | O(n) |

*Amortized

## Common Variations

1. **Output-Restricted Deque**
   - Insert at both ends
   - Delete only from front
   - Useful for certain scheduling algorithms

2. **Input-Restricted Deque**
   - Insert only at rear
   - Delete from both ends
   - Less common

3. **Monotonic Deque**
   - Maintains increasing or decreasing order
   - Key technique for sliding window problems
   - Elements that violate monotonicity are removed

4. **Concurrent Deque**
   - Thread-safe with lock-free operations
   - Used in work-stealing schedulers

## Common Interview Problems

1. **Sliding Window Maximum**: Classic monotonic deque problem
2. **Design Circular Deque**: LeetCode 641
3. **Shortest Subarray with Sum at Least K**: Monotonic deque + prefix sums
4. **Maximum of Minimum for Every Window Size**: Advanced deque application
5. **Design Front Middle Back Queue**: LeetCode 1670

## Implementation Tips

### When to Use Array vs. Linked List

**Use Circular Array when:**
- Need random access by index
- Memory locality matters (embedded systems)
- Know maximum size in advance

**Use Doubly Linked List when:**
- Size varies dramatically
- Never need random access
- Memory fragmentation is acceptable

### Common Pitfalls

1. **Off-by-one errors** in modular arithmetic
2. **Empty deque handling** - check before pop
3. **Forgetting to update size** counter
4. **Resize direction** - must copy maintaining order

## Further Reading

- CLRS Chapter 10.1 (Stacks and Queues)
- [Deque - Wikipedia](https://en.wikipedia.org/wiki/Double-ended_queue)
- [Visualgo - Deque](https://visualgo.net/en/list)
- [Python deque documentation](https://docs.python.org/3/library/collections.html#collections.deque)
