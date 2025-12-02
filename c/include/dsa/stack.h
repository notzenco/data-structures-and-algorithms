/**
 * @file stack.h
 * @brief Stack data structure (LIFO) - dynamic array implementation
 *
 * A stack is a linear data structure that follows the Last-In-First-Out (LIFO)
 * principle. Elements are added and removed from the same end (top).
 *
 * Time Complexity:
 *   - push:    O(1) amortized
 *   - pop:     O(1)
 *   - peek:    O(1)
 *   - isEmpty: O(1)
 *   - size:    O(1)
 *
 * Space Complexity: O(n) where n is the number of elements
 */

#ifndef DSA_STACK_H
#define DSA_STACK_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque stack type */
typedef struct Stack Stack;

/** Result codes for stack operations */
typedef enum {
    STACK_OK = 0,
    STACK_ERR_NULL,      /* NULL pointer argument */
    STACK_ERR_EMPTY,     /* Operation on empty stack */
    STACK_ERR_ALLOC      /* Memory allocation failed */
} StackResult;

/**
 * Create a new stack with specified initial capacity.
 *
 * @param capacity Initial capacity (0 for default)
 * @return Pointer to new stack, or NULL on allocation failure
 */
Stack *stack_create(size_t capacity);

/**
 * Destroy stack and free all memory.
 *
 * @param stack Stack to destroy (NULL safe)
 */
void stack_destroy(Stack *stack);

/**
 * Push an element onto the stack.
 *
 * @param stack Target stack
 * @param value Value to push
 * @return STACK_OK on success, error code otherwise
 */
StackResult stack_push(Stack *stack, int value);

/**
 * Pop an element from the stack.
 *
 * @param stack Target stack
 * @param out   Pointer to store popped value (can be NULL to discard)
 * @return STACK_OK on success, STACK_ERR_EMPTY if stack is empty
 */
StackResult stack_pop(Stack *stack, int *out);

/**
 * Peek at the top element without removing it.
 *
 * @param stack Target stack
 * @param out   Pointer to store top value
 * @return STACK_OK on success, STACK_ERR_EMPTY if stack is empty
 */
StackResult stack_peek(const Stack *stack, int *out);

/**
 * Check if stack is empty.
 *
 * @param stack Target stack
 * @return true if empty or NULL, false otherwise
 */
bool stack_is_empty(const Stack *stack);

/**
 * Get the number of elements in the stack.
 *
 * @param stack Target stack
 * @return Number of elements, 0 if NULL
 */
size_t stack_size(const Stack *stack);

/**
 * Remove all elements from the stack.
 *
 * @param stack Target stack
 * @return STACK_OK on success, STACK_ERR_NULL if stack is NULL
 */
StackResult stack_clear(Stack *stack);

#endif /* DSA_STACK_H */
