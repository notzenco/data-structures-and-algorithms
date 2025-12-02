# Queue

## Overview

A queue is a linear data structure that follows the **First-In-First-Out (FIFO)** principle. The first element added to the queue is the first one to be removed. Think of it as a line of people waiting: the person who arrived first gets served first.

## Intuition

```
    Dequeue ←  ┌───┬───┬───┬───┐  ← Enqueue
    (front)    │ 1 │ 2 │ 3 │ 4 │    (rear)
               └───┴───┴───┴───┘
               front           rear

Enqueue(5):
               ┌───┬───┬───┬───┬───┐
               │ 1 │ 2 │ 3 │ 4 │ 5 │
               └───┴───┴───┴───┴───┘

Dequeue() → returns 1:
               ┌───┬───┬───┬───┐
               │ 2 │ 3 │ 4 │ 5 │
               └───┴───┴───┴───┘
```

## How It Works

### Core Operations

```
enqueue(element):
    add element to rear of queue

dequeue():
    if isEmpty():
        error "Queue underflow"
    remove and return front element

front() / peek():
    if isEmpty():
        error "Queue is empty"
    return front element without removing

isEmpty():
    return size == 0
```

### Naive Array Implementation (Inefficient)

```
Queue:
    array[]
    size = 0

enqueue(element):
    array[size] = element
    size = size + 1

dequeue():                    // O(n) - BAD!
    element = array[0]
    shift all elements left
    size = size - 1
    return element
```

### Circular Array Implementation (Efficient)

```
Queue:
    array[capacity]
    front = 0
    rear = 0
    size = 0

enqueue(element):
    if size == capacity:
        resize()  // or error
    array[rear] = element
    rear = (rear + 1) % capacity
    size = size + 1

dequeue():
    element = array[front]
    front = (front + 1) % capacity
    size = size - 1
    return element
```

**Circular Array Visualization:**
```
Initial: capacity=5, front=0, rear=0
         [_][_][_][_][_]
          ↑
        front,rear

After enqueue(A,B,C): front=0, rear=3
         [A][B][C][_][_]
          ↑        ↑
        front    rear

After dequeue() twice: front=2, rear=3
         [_][_][C][_][_]
                ↑  ↑
             front rear

After enqueue(D,E,F): front=2, rear=1 (wrapped!)
         [F][_][C][D][E]
             ↑  ↑
           rear front
```

### Linked List Implementation

```
Queue:
    head = null  // front
    tail = null  // rear

enqueue(element):
    new_node = Node(element)
    if tail == null:
        head = tail = new_node
    else:
        tail.next = new_node
        tail = new_node

dequeue():
    element = head.data
    head = head.next
    if head == null:
        tail = null
    return element
```

## Mathematical Analysis

### Why Circular Array?

Without circular indexing, dequeue would require shifting all elements:
- n-1 elements shifted for first dequeue
- n-2 for second, etc.
- Total for n dequeues: (n-1) + (n-2) + ... + 1 = n(n-1)/2 = O(n²)

With circular indexing:
- Just increment front pointer with modulo
- O(1) per operation, O(n) total for n operations

### Modulo Arithmetic

The key formula: `index = (index + 1) % capacity`

This creates wrap-around behavior:
```
capacity = 5
index:  0 → 1 → 2 → 3 → 4 → 0 → 1 → ...
```

## Time Complexity

| Operation | Array (Circular) | Linked List |
|-----------|------------------|-------------|
| enqueue() | O(1)*           | O(1)        |
| dequeue() | O(1)            | O(1)        |
| front()   | O(1)            | O(1)        |
| isEmpty() | O(1)            | O(1)        |
| size()    | O(1)            | O(1)        |

*O(1) amortized if dynamic resizing is used

## Space Complexity

- **Circular array**: O(n), may have unused capacity
- **Linked list**: O(n), overhead of one pointer per node

## Use Cases

### Classic Applications

1. **CPU Scheduling**
   - Processes wait in a queue for CPU time
   - Round-robin scheduling uses circular queue

2. **Breadth-First Search (BFS)**
   ```
   BFS(start):
       queue.enqueue(start)
       while not queue.isEmpty():
           node = queue.dequeue()
           for neighbor in node.neighbors:
               if not visited[neighbor]:
                   queue.enqueue(neighbor)
   ```

3. **Print Spooler**
   - Print jobs queue up in order received
   - First submitted = first printed

4. **Message Queues**
   - Kafka, RabbitMQ, SQS
   - Producer enqueues messages
   - Consumer dequeues and processes

5. **Buffering**
   - Video streaming: buffer frames in queue
   - Keyboard input: buffer keystrokes

6. **Level-Order Tree Traversal**
   ```
           1
          / \
         2   3
        / \
       4   5

   Queue: [1] → [2,3] → [3,4,5] → [4,5] → [5] → []
   Output: 1, 2, 3, 4, 5
   ```

### Real-World Examples

- **Operating systems**: Process scheduling, I/O request handling
- **Web servers**: Request queues
- **Customer service**: Call center hold queues
- **Printers**: Print job management
- **Networks**: Packet routing, traffic management

## Trade-offs & Comparisons

### Array vs. Linked List Implementation

| Aspect | Circular Array | Linked List |
|--------|----------------|-------------|
| Cache locality | Excellent | Poor |
| Memory overhead | May waste capacity | Pointer per node |
| Implementation | Moderate (modulo logic) | Simple |
| Fixed capacity | Optional | Never needed |

### Queue vs. Stack

| Aspect | Queue (FIFO) | Stack (LIFO) |
|--------|--------------|--------------|
| Order | First in, first out | Last in, first out |
| Primary use | BFS, scheduling | DFS, recursion |
| Access point | Two ends | One end |

### Queue vs. Deque

| Aspect | Queue | Deque |
|--------|-------|-------|
| Insert | Rear only | Both ends |
| Remove | Front only | Both ends |
| Flexibility | Limited | High |
| Use case | Pure FIFO | Versatile |

## Common Variations

1. **Circular Queue (Ring Buffer)**
   - Fixed capacity with wrap-around
   - No resizing, predictable memory
   - Used in embedded systems, audio/video buffering

2. **Priority Queue**
   - Elements dequeued by priority, not arrival order
   - Usually implemented with a heap
   - Used in Dijkstra's algorithm, task scheduling

3. **Double-Ended Queue (Deque)**
   - Insert/remove from both ends
   - Can act as both stack and queue

4. **Blocking Queue**
   - Thread-safe with blocking operations
   - Enqueue blocks when full, dequeue blocks when empty
   - Used in producer-consumer patterns

## Common Interview Problems

1. **Implement Queue using Stacks**: Use two stacks
2. **Implement Stack using Queues**: Use two queues
3. **Moving Average from Data Stream**: Sliding window queue
4. **Design Hit Counter**: Time-based queue
5. **Number of Recent Calls**: Count calls in time window
6. **Rotting Oranges**: Multi-source BFS

## Further Reading

- CLRS Chapter 10.1 (Stacks and Queues)
- [Queue - Wikipedia](https://en.wikipedia.org/wiki/Queue_(abstract_data_type))
- [Visualgo - Queue](https://visualgo.net/en/list)
