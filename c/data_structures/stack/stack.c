/**
 * @file stack.c
 * @brief Stack implementation using a dynamic array
 */

#include "stack.h"
#include <stdlib.h>

#define DEFAULT_CAPACITY 16

/** Stack internal structure */
struct Stack {
    int *data;
    size_t size;
    size_t capacity;
};

/**
 * Resize the stack's internal array.
 * @param stack Stack to resize
 * @param new_capacity New capacity
 * @return STACK_OK on success, STACK_ERR_ALLOC on failure
 */
static StackResult stack_resize(Stack *stack, size_t new_capacity) {
    int *new_data = realloc(stack->data, new_capacity * sizeof(int));
    if (!new_data) {
        return STACK_ERR_ALLOC;
    }
    stack->data = new_data;
    stack->capacity = new_capacity;
    return STACK_OK;
}

Stack *stack_create(size_t capacity) {
    Stack *stack = malloc(sizeof(Stack));
    if (!stack) {
        return NULL;
    }

    if (capacity == 0) {
        capacity = DEFAULT_CAPACITY;
    }

    stack->data = malloc(capacity * sizeof(int));
    if (!stack->data) {
        free(stack);
        return NULL;
    }

    stack->size = 0;
    stack->capacity = capacity;
    return stack;
}

void stack_destroy(Stack *stack) {
    if (stack) {
        free(stack->data);
        free(stack);
    }
}

StackResult stack_push(Stack *stack, int value) {
    if (!stack) {
        return STACK_ERR_NULL;
    }

    /* Grow if needed */
    if (stack->size == stack->capacity) {
        StackResult result = stack_resize(stack, stack->capacity * 2);
        if (result != STACK_OK) {
            return result;
        }
    }

    stack->data[stack->size++] = value;
    return STACK_OK;
}

StackResult stack_pop(Stack *stack, int *out) {
    if (!stack) {
        return STACK_ERR_NULL;
    }
    if (stack->size == 0) {
        return STACK_ERR_EMPTY;
    }

    stack->size--;
    if (out) {
        *out = stack->data[stack->size];
    }
    return STACK_OK;
}

StackResult stack_peek(const Stack *stack, int *out) {
    if (!stack) {
        return STACK_ERR_NULL;
    }
    if (stack->size == 0) {
        return STACK_ERR_EMPTY;
    }
    if (out) {
        *out = stack->data[stack->size - 1];
    }
    return STACK_OK;
}

bool stack_is_empty(const Stack *stack) {
    return !stack || stack->size == 0;
}

size_t stack_size(const Stack *stack) {
    return stack ? stack->size : 0;
}

StackResult stack_clear(Stack *stack) {
    if (!stack) {
        return STACK_ERR_NULL;
    }
    stack->size = 0;
    return STACK_OK;
}
