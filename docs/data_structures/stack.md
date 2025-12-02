# Stack

## Overview

A stack is a linear data structure that follows the **Last-In-First-Out (LIFO)** principle. The last element added to the stack is the first one to be removed. Think of it as a stack of plates: you can only add or remove plates from the top.

## Intuition

```
        │   │
        │ 3 │ ← top (most recently added)
        ├───┤
        │ 2 │
        ├───┤
        │ 1 │ ← bottom (first added)
        └───┘

Push(4):        Pop():
        │   │           │   │
        │ 4 │ ← new     │   │
        ├───┤           ├───┤
        │ 3 │           │ 3 │ ← new top
        ├───┤           ├───┤
        │ 2 │           │ 2 │
        ├───┤           ├───┤
        │ 1 │           │ 1 │
        └───┘           └───┘
```

The stack provides a restricted interface intentionally: by limiting access to only the top element, we get simplicity and guaranteed O(1) operations.

## How It Works

### Core Operations

```
push(element):
    add element to top of stack

pop():
    if isEmpty():
        error "Stack underflow"
    remove and return top element

peek() / top():
    if isEmpty():
        error "Stack is empty"
    return top element without removing

isEmpty():
    return size == 0
```

### Array-Based Implementation

```
Stack:
    array[]
    top = -1  // Index of top element, -1 when empty

push(element):
    top = top + 1
    array[top] = element

pop():
    element = array[top]
    top = top - 1
    return element

peek():
    return array[top]
```

### Linked List Implementation

```
Stack:
    head = null  // Points to top element

push(element):
    new_node = Node(element)
    new_node.next = head
    head = new_node

pop():
    element = head.data
    head = head.next
    return element
```

## Mathematical Analysis

### Why O(1) for All Operations?

Each operation involves:
- **Push**: One assignment (or allocation + pointer update)
- **Pop**: One read + one decrement (or pointer update)
- **Peek**: One read

No loops, no recursion, no dependency on stack size → O(1)

### Stack Depth in Recursion

When a recursive algorithm uses a stack (explicitly or via call stack), the space complexity equals the maximum stack depth.

For example, in binary tree traversal:
- **Balanced tree**: Stack depth = O(log n)
- **Skewed tree**: Stack depth = O(n)

## Time Complexity

| Operation | Time | Notes |
|-----------|------|-------|
| push()    | O(1) | O(1) amortized if using dynamic array |
| pop()     | O(1) | |
| peek()    | O(1) | |
| isEmpty() | O(1) | |
| size()    | O(1) | If maintained as a counter |
| search()  | O(n) | Not a standard stack operation |

## Space Complexity

- **Array-based**: O(n) for n elements, potential wasted capacity
- **Linked list-based**: O(n) with per-element overhead for pointers

## Use Cases

### Classic Applications

1. **Function Call Stack**
   - Every function call pushes a frame (return address, local variables)
   - Return pops the frame
   - This is why "stack overflow" crashes happen with deep recursion

2. **Expression Evaluation**
   ```
   Infix:   3 + 4 * 2
   Postfix: 3 4 2 * +

   Evaluate postfix with stack:
   Push 3: [3]
   Push 4: [3, 4]
   Push 2: [3, 4, 2]
   Mul:    [3, 8]      // pop 4,2; push 4*2
   Add:    [11]        // pop 3,8; push 3+8
   ```

3. **Parentheses Matching**
   ```
   Input: "({[]})"

   (  → push     stack: [(]
   {  → push     stack: [(, {]
   [  → push     stack: [(, {, []
   ]  → pop [    stack: [(, {]     ✓ matches
   }  → pop {    stack: [(]        ✓ matches
   )  → pop (    stack: []         ✓ matches
   End: stack empty → Valid!
   ```

4. **Undo/Redo Functionality**
   - Each action pushes to undo stack
   - Undo pops from undo stack, pushes to redo stack
   - Redo pops from redo stack, pushes to undo stack

5. **Browser History (Back Button)**
   - Each page visit pushes URL to back stack
   - Back button pops from back stack, pushes to forward stack

6. **DFS (Depth-First Search)**
   - Explicit stack replaces recursion
   - Enables iterative tree/graph traversal

### Real-World Examples

- **Text editors**: Undo/redo operations
- **Compilers**: Syntax parsing, expression evaluation
- **Memory management**: Call stack in virtually all programming languages
- **Backtracking algorithms**: Maze solving, puzzle solving
- **Syntax highlighting**: Bracket matching in IDEs

## Trade-offs & Comparisons

### Array vs. Linked List Implementation

| Aspect | Array-Based | Linked List |
|--------|-------------|-------------|
| Memory | Contiguous, cache-friendly | Scattered, pointer overhead |
| Resize | Occasional O(n) copy | Never needed |
| Memory efficiency | May waste capacity | Exact fit |
| Implementation | Simpler | Slightly more complex |

### Stack vs. Queue

| Aspect | Stack (LIFO) | Queue (FIFO) |
|--------|--------------|--------------|
| Order | Last in, first out | First in, first out |
| Use case | Backtracking, recursion | Scheduling, BFS |
| Analogy | Stack of plates | Line at a store |

## Common Variations

1. **Min Stack**: O(1) access to minimum element
   - Maintain auxiliary stack of minimums

2. **Max Stack**: O(1) access to maximum element
   - Similar to min stack

3. **Two Stacks in One Array**: Space optimization
   - Stack 1 grows from left
   - Stack 2 grows from right

4. **Stack with Middle Operation**: O(1) access to middle
   - Use doubly linked list with middle pointer

## Common Interview Problems

1. **Valid Parentheses**: Check if brackets are balanced
2. **Min Stack**: Design stack with O(1) getMin()
3. **Evaluate Reverse Polish Notation**: Postfix expression evaluation
4. **Daily Temperatures**: Next greater element problem
5. **Largest Rectangle in Histogram**: Monotonic stack application
6. **Implement Queue using Stacks**: Classic design problem

## Further Reading

- CLRS Chapter 10.1 (Stacks and Queues)
- [Stack - Wikipedia](https://en.wikipedia.org/wiki/Stack_(abstract_data_type))
- [Visualgo - Stack](https://visualgo.net/en/list)
