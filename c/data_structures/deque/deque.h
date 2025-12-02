/**
 * @file deque.h
 * @brief Double-ended queue (deque) interface
 *
 * A deque allows insertion and removal from both ends in O(1) time.
 * Implemented using a circular dynamic array.
 */

#ifndef DSA_DEQUE_H
#define DSA_DEQUE_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque deque type */
typedef struct Deque Deque;

/** Result codes for deque operations */
typedef enum {
    DEQUE_OK = 0,       /**< Operation successful */
    DEQUE_ERR_NULL,     /**< NULL pointer argument */
    DEQUE_ERR_EMPTY,    /**< Deque is empty */
    DEQUE_ERR_INDEX,    /**< Index out of bounds */
    DEQUE_ERR_ALLOC     /**< Memory allocation failed */
} DequeResult;

/**
 * Create a new deque.
 * @param capacity Initial capacity (0 for default)
 * @return New deque or NULL on allocation failure
 */
Deque *deque_create(size_t capacity);

/**
 * Destroy a deque and free all memory.
 * @param deque Deque to destroy (NULL safe)
 */
void deque_destroy(Deque *deque);

/**
 * Add a value to the front of the deque.
 * @param deque Target deque
 * @param value Value to add
 * @return DEQUE_OK on success, error code otherwise
 */
DequeResult deque_push_front(Deque *deque, int value);

/**
 * Add a value to the back of the deque.
 * @param deque Target deque
 * @param value Value to add
 * @return DEQUE_OK on success, error code otherwise
 */
DequeResult deque_push_back(Deque *deque, int value);

/**
 * Remove and return the front element.
 * @param deque Target deque
 * @param out Pointer to store removed value (can be NULL)
 * @return DEQUE_OK on success, DEQUE_ERR_EMPTY if empty
 */
DequeResult deque_pop_front(Deque *deque, int *out);

/**
 * Remove and return the back element.
 * @param deque Target deque
 * @param out Pointer to store removed value (can be NULL)
 * @return DEQUE_OK on success, DEQUE_ERR_EMPTY if empty
 */
DequeResult deque_pop_back(Deque *deque, int *out);

/**
 * Get the front value without removing it.
 * @param deque Target deque
 * @param out Pointer to store value
 * @return DEQUE_OK on success, DEQUE_ERR_EMPTY if empty
 */
DequeResult deque_front(const Deque *deque, int *out);

/**
 * Get the back value without removing it.
 * @param deque Target deque
 * @param out Pointer to store value
 * @return DEQUE_OK on success, DEQUE_ERR_EMPTY if empty
 */
DequeResult deque_back(const Deque *deque, int *out);

/**
 * Get value at index (0 = front).
 * @param deque Target deque
 * @param index Index to access
 * @param out Pointer to store value
 * @return DEQUE_OK on success, DEQUE_ERR_INDEX if out of bounds
 */
DequeResult deque_get(const Deque *deque, size_t index, int *out);

/**
 * Set value at index.
 * @param deque Target deque
 * @param index Index to modify
 * @param value New value
 * @return DEQUE_OK on success, DEQUE_ERR_INDEX if out of bounds
 */
DequeResult deque_set(Deque *deque, size_t index, int value);

/**
 * Get the number of elements in the deque.
 * @param deque Target deque
 * @return Number of elements, or 0 if NULL
 */
size_t deque_size(const Deque *deque);

/**
 * Check if the deque is empty.
 * @param deque Target deque
 * @return true if empty or NULL, false otherwise
 */
bool deque_is_empty(const Deque *deque);

/**
 * Remove all elements from the deque.
 * @param deque Target deque
 * @return DEQUE_OK on success, DEQUE_ERR_NULL if NULL
 */
DequeResult deque_clear(Deque *deque);

#endif /* DSA_DEQUE_H */
