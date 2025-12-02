/**
 * @file heap.h
 * @brief Binary Min Heap interface
 *
 * A complete binary tree where each node is smaller than its children.
 * Provides O(log n) insertion and O(log n) removal of minimum element.
 * Implemented using a dynamic array.
 */

#ifndef DSA_HEAP_H
#define DSA_HEAP_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque heap type */
typedef struct Heap Heap;

/** Result codes for heap operations */
typedef enum {
    HEAP_OK = 0,        /**< Operation successful */
    HEAP_ERR_NULL,      /**< NULL pointer argument */
    HEAP_ERR_EMPTY,     /**< Heap is empty */
    HEAP_ERR_ALLOC      /**< Memory allocation failed */
} HeapResult;

/**
 * Create a new min heap.
 * @param capacity Initial capacity (0 for default)
 * @return New heap or NULL on allocation failure
 */
Heap *heap_create(size_t capacity);

/**
 * Destroy a heap and free all memory.
 * @param heap Heap to destroy (NULL safe)
 */
void heap_destroy(Heap *heap);

/**
 * Insert a value into the heap.
 * @param heap Target heap
 * @param value Value to insert
 * @return HEAP_OK on success, error code otherwise
 */
HeapResult heap_push(Heap *heap, int value);

/**
 * Remove and return the minimum value.
 * @param heap Target heap
 * @param out Pointer to store removed value (can be NULL)
 * @return HEAP_OK on success, HEAP_ERR_EMPTY if empty
 */
HeapResult heap_pop(Heap *heap, int *out);

/**
 * Get the minimum value without removing it.
 * @param heap Target heap
 * @param out Pointer to store value
 * @return HEAP_OK on success, HEAP_ERR_EMPTY if empty
 */
HeapResult heap_peek(const Heap *heap, int *out);

/**
 * Get the number of elements in the heap.
 * @param heap Target heap
 * @return Number of elements, or 0 if NULL
 */
size_t heap_size(const Heap *heap);

/**
 * Check if the heap is empty.
 * @param heap Target heap
 * @return true if empty or NULL, false otherwise
 */
bool heap_is_empty(const Heap *heap);

/**
 * Remove all elements from the heap.
 * @param heap Target heap
 * @return HEAP_OK on success, HEAP_ERR_NULL if NULL
 */
HeapResult heap_clear(Heap *heap);

/**
 * Build a heap from an array of values.
 * @param values Array of values
 * @param count Number of values
 * @return New heap or NULL on allocation failure
 */
Heap *heap_from_array(const int *values, size_t count);

#endif /* DSA_HEAP_H */
