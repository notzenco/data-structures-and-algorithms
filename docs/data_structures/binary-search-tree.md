# Binary Search Tree (BST)

## Overview

A Binary Search Tree is a hierarchical data structure where each node has at most two children. The key property: for any node, all keys in its left subtree are **smaller**, and all keys in its right subtree are **larger**. This ordering enables efficient O(log n) search, insert, and delete operations.

## Intuition

```
              8
            /   \
           3     10
          / \      \
         1   6      14
            / \     /
           4   7   13

BST Property:
- Everything left of 8 is < 8
- Everything right of 8 is > 8
- This holds recursively for every node

Search for 6:
8 → 6 < 8, go left
3 → 6 > 3, go right
6 → Found!

Only 3 comparisons for a tree of 8 nodes.
```

## How It Works

### Node Structure

```
Node:
    key
    value (optional)
    left   // Left child
    right  // Right child
    parent // Optional, simplifies some operations
```

### Search

```
search(node, target):
    if node == null:
        return NOT_FOUND
    if target == node.key:
        return node
    if target < node.key:
        return search(node.left, target)
    else:
        return search(node.right, target)
```

```
Search for 7 in:
        8
       / \
      3   10
     / \
    1   6
       / \
      4   7

8 → 7 < 8, go left
3 → 7 > 3, go right
6 → 7 > 6, go right
7 → Found!
```

### Insert

```
insert(node, key, value):
    if node == null:
        return new Node(key, value)
    if key < node.key:
        node.left = insert(node.left, key, value)
    else if key > node.key:
        node.right = insert(node.right, key, value)
    else:
        node.value = value  // Update existing
    return node
```

```
Insert 5 into:
        8               8
       / \             / \
      3   10    →     3   10
     / \             / \
    1   6           1   6
                       /
                      5
```

### Delete

Three cases:
1. **Leaf node**: Simply remove
2. **One child**: Replace with child
3. **Two children**: Replace with successor (or predecessor)

```
delete(node, key):
    if node == null:
        return null

    if key < node.key:
        node.left = delete(node.left, key)
    else if key > node.key:
        node.right = delete(node.right, key)
    else:
        // Found the node to delete
        if node.left == null:
            return node.right
        if node.right == null:
            return node.left
        // Two children: replace with in-order successor
        successor = find_min(node.right)
        node.key = successor.key
        node.right = delete(node.right, successor.key)

    return node

find_min(node):
    while node.left != null:
        node = node.left
    return node
```

```
Delete 3 (two children):
        8                    8
       / \                  / \
      3   10       →       4   10
     / \                  / \
    1   6                1   6
       /
      4

Successor of 3 is 4 (leftmost in right subtree)
Replace 3 with 4, delete original 4
```

### Traversals

**In-Order (sorted order)**
```
in_order(node):
    if node == null: return
    in_order(node.left)
    visit(node)           // Process in sorted order
    in_order(node.right)
```

**Pre-Order (root first)**
```
pre_order(node):
    if node == null: return
    visit(node)           // Process root first
    pre_order(node.left)
    pre_order(node.right)
```

**Post-Order (root last)**
```
post_order(node):
    if node == null: return
    post_order(node.left)
    post_order(node.right)
    visit(node)           // Process root last
```

## Mathematical Analysis

### Height and Balance

**Perfect binary tree** (all levels full):
- Height h has 2^(h+1) - 1 nodes
- n nodes → height = log₂(n+1) - 1 ≈ O(log n)

**Balanced BST**:
- Height maintained at O(log n)
- All operations O(log n)

**Skewed BST** (worst case):
- Inserting sorted data: 1, 2, 3, 4, 5
```
1
 \
  2
   \
    3
     \
      4
       \
        5
```
- Degenerates to linked list
- Height = n - 1, operations become O(n)

### Number of BSTs

Given n distinct keys, the number of structurally different BSTs is the **Catalan number**:

C(n) = (2n)! / ((n+1)! × n!)

| n | C(n) |
|---|------|
| 1 | 1    |
| 2 | 2    |
| 3 | 5    |
| 4 | 14   |
| 5 | 42   |

### Expected Height (Random Insertions)

For n keys inserted in random order:
- Expected height ≈ 2.99 × log₂(n)
- Still O(log n), but with constant factor

## Time Complexity

| Operation | Average | Worst Case |
|-----------|---------|------------|
| Search    | O(log n)| O(n)       |
| Insert    | O(log n)| O(n)       |
| Delete    | O(log n)| O(n)       |
| Min/Max   | O(log n)| O(n)       |
| Successor | O(log n)| O(n)       |
| In-order  | O(n)    | O(n)       |

Worst case occurs with skewed trees.

## Space Complexity

- **Storage**: O(n) for n nodes
- **Per node**: Key + value + 2-3 pointers
- **Recursion stack**: O(h) where h is height
  - O(log n) for balanced, O(n) for skewed

## Use Cases

### When to Use BSTs

1. **Need sorted data with dynamic updates**
   - Maintain sorted order as elements added/removed
   - Range queries

2. **Ordered operations**
   - Find k-th smallest element
   - Find predecessor/successor
   - Range queries

3. **Database indexing**
   - B-trees (generalized BST) for disk-based storage

### Real-World Examples

- **File systems**: Directory hierarchies
- **Databases**: B-tree indexes
- **Compilers**: Expression trees
- **Graphics**: BSP trees for rendering
- **Network routing**: Lookup tables

### When NOT to Use

- Don't need ordering → Use hash table (faster)
- Worst-case guarantees needed → Use balanced BST
- Memory-constrained → Consider other structures

## Trade-offs & Comparisons

### BST vs. Hash Table

| Aspect | BST | Hash Table |
|--------|-----|------------|
| Search | O(log n) | O(1) avg |
| Ordered iteration | Yes | No |
| Range queries | O(log n + k) | O(n) |
| Predecessor/Successor | O(log n) | O(n) |
| Worst case | O(n) | O(n) |
| Memory overhead | 2-3 pointers/node | ~50% extra capacity |

### BST vs. Sorted Array

| Aspect | BST | Sorted Array |
|--------|-----|--------------|
| Search | O(log n) | O(log n) |
| Insert | O(log n)* | O(n) |
| Delete | O(log n)* | O(n) |
| Space | Higher | Lower |
| Cache | Poor | Excellent |

*Average case, O(n) worst case

## Common Variations

1. **Self-Balancing BSTs**
   - **AVL Tree**: Strict balance (height diff ≤ 1)
   - **Red-Black Tree**: Relaxed balance, fewer rotations
   - **Splay Tree**: Self-adjusting, good cache locality
   - **Treap**: Randomized, expected O(log n)

2. **Augmented BSTs**
   - Store additional information at nodes
   - Order statistics tree (rank, select)
   - Interval tree (overlapping intervals)

3. **B-Trees**
   - Generalized BST with multiple keys per node
   - Optimized for disk access
   - Used in databases and file systems

4. **BST with Parent Pointers**
   - Enables O(1) successor finding with a pointer
   - More memory, simpler some algorithms

## Common Interview Problems

1. **Validate BST**: Check if tree satisfies BST property
2. **Kth Smallest Element**: In-order traversal or augmented tree
3. **Lowest Common Ancestor**: Use BST property
4. **Convert Sorted Array to BST**: Build balanced tree
5. **Serialize/Deserialize BST**: Pre-order traversal
6. **Range Sum in BST**: Traverse relevant subtrees
7. **Two Sum in BST**: Iterator + two-pointer

### Validating a BST

```
is_valid_bst(node, min_val, max_val):
    if node == null:
        return true
    if node.key <= min_val or node.key >= max_val:
        return false
    return is_valid_bst(node.left, min_val, node.key) and
           is_valid_bst(node.right, node.key, max_val)

// Initial call:
is_valid_bst(root, -∞, +∞)
```

## Implementation Tips

### Handling Duplicates

Options:
1. **Don't allow**: Update value if key exists
2. **Count field**: Track count of each key
3. **Go left or right**: Consistently place duplicates on one side
4. **Linked list**: Chain duplicates at each node

### Iterative vs. Recursive

**Recursive**: Cleaner, more intuitive
```python
def search(node, key):
    if not node or node.key == key:
        return node
    if key < node.key:
        return search(node.left, key)
    return search(node.right, key)
```

**Iterative**: No stack overflow, sometimes faster
```python
def search(root, key):
    node = root
    while node and node.key != key:
        if key < node.key:
            node = node.left
        else:
            node = node.right
    return node
```

## Further Reading

- CLRS Chapter 12 (Binary Search Trees)
- [BST - Wikipedia](https://en.wikipedia.org/wiki/Binary_search_tree)
- [Visualgo - BST](https://visualgo.net/en/bst)
- [Self-Balancing BST - Wikipedia](https://en.wikipedia.org/wiki/Self-balancing_binary_search_tree)
