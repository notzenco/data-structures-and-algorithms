# C Implementations

Data structures and algorithms implemented in C11.

## Building

```bash
cmake -S . -B build
cmake --build build
```

## Running Tests

```bash
ctest --test-dir build --output-on-failure
```

## Structure

```
c/
├── CMakeLists.txt
├── include/
│   └── dsa/          # Header files
├── src/              # Implementation files
└── tests/            # Test files
```

## Implementations

| Component | Status | Header | Source |
|-----------|--------|--------|--------|
| Stack | ✅ | [stack.h](include/dsa/stack.h) | [stack.c](src/stack.c) |
| Queue | ❌ | - | - |
| Dynamic Array | ❌ | - | - |
| Singly Linked List | ❌ | - | - |
| Doubly Linked List | ❌ | - | - |
| Deque | ❌ | - | - |
| Hash Table | ❌ | - | - |
| Binary Search Tree | ❌ | - | - |
| Binary Min Heap | ❌ | - | - |
| Disjoint Set | ❌ | - | - |
