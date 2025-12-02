/**
 * @file dynamic_array.h
 * @brief Dynamic array (vector) interface
 *
 * A resizable array that grows automatically when capacity is exceeded.
 * Provides O(1) amortized insertion at the end and O(1) random access.
 */

#ifndef DSA_DYNAMIC_ARRAY_H
#define DSA_DYNAMIC_ARRAY_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque dynamic array type */
typedef struct DynamicArray DynamicArray;

/** Result codes for dynamic array operations */
typedef enum {
    DA_OK = 0,          /**< Operation successful */
    DA_ERR_NULL,        /**< NULL pointer argument */
    DA_ERR_INDEX,       /**< Index out of bounds */
    DA_ERR_EMPTY,       /**< Array is empty */
    DA_ERR_ALLOC        /**< Memory allocation failed */
} DAResult;

/**
 * Create a new dynamic array.
 * @param capacity Initial capacity (0 for default)
 * @return New array or NULL on allocation failure
 */
DynamicArray *da_create(size_t capacity);

/**
 * Destroy a dynamic array and free all memory.
 * @param array Array to destroy (NULL safe)
 */
void da_destroy(DynamicArray *array);

/**
 * Append a value to the end of the array.
 * @param array Target array
 * @param value Value to append
 * @return DA_OK on success, error code otherwise
 */
DAResult da_push(DynamicArray *array, int value);

/**
 * Remove and return the last element.
 * @param array Target array
 * @param out Pointer to store removed value (can be NULL)
 * @return DA_OK on success, DA_ERR_EMPTY if empty
 */
DAResult da_pop(DynamicArray *array, int *out);

/**
 * Get value at index.
 * @param array Target array
 * @param index Index to access
 * @param out Pointer to store value
 * @return DA_OK on success, DA_ERR_INDEX if out of bounds
 */
DAResult da_get(const DynamicArray *array, size_t index, int *out);

/**
 * Set value at index.
 * @param array Target array
 * @param index Index to modify
 * @param value New value
 * @return DA_OK on success, DA_ERR_INDEX if out of bounds
 */
DAResult da_set(DynamicArray *array, size_t index, int value);

/**
 * Insert value at index, shifting elements right.
 * @param array Target array
 * @param index Index to insert at (0 to size inclusive)
 * @param value Value to insert
 * @return DA_OK on success, error code otherwise
 */
DAResult da_insert(DynamicArray *array, size_t index, int value);

/**
 * Remove value at index, shifting elements left.
 * @param array Target array
 * @param index Index to remove
 * @param out Pointer to store removed value (can be NULL)
 * @return DA_OK on success, DA_ERR_INDEX if out of bounds
 */
DAResult da_remove(DynamicArray *array, size_t index, int *out);

/**
 * Get the number of elements in the array.
 * @param array Target array
 * @return Number of elements, or 0 if NULL
 */
size_t da_size(const DynamicArray *array);

/**
 * Get the current capacity of the array.
 * @param array Target array
 * @return Current capacity, or 0 if NULL
 */
size_t da_capacity(const DynamicArray *array);

/**
 * Check if the array is empty.
 * @param array Target array
 * @return true if empty or NULL, false otherwise
 */
bool da_is_empty(const DynamicArray *array);

/**
 * Remove all elements from the array.
 * @param array Target array
 * @return DA_OK on success, DA_ERR_NULL if NULL
 */
DAResult da_clear(DynamicArray *array);

/**
 * Resize the array to fit exactly its current size.
 * @param array Target array
 * @return DA_OK on success, error code otherwise
 */
DAResult da_shrink_to_fit(DynamicArray *array);

#endif /* DSA_DYNAMIC_ARRAY_H */
