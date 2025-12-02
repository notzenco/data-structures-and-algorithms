/**
 * @file doubly_linked_list.h
 * @brief Doubly linked list interface
 *
 * A linked list where each node contains pointers to both next and previous nodes.
 * Provides O(1) insertion/deletion at both ends and O(n) access by index.
 */

#ifndef DSA_DOUBLY_LINKED_LIST_H
#define DSA_DOUBLY_LINKED_LIST_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque doubly linked list type */
typedef struct DoublyLinkedList DoublyLinkedList;

/** Result codes for doubly linked list operations */
typedef enum {
    DLL_OK = 0,         /**< Operation successful */
    DLL_ERR_NULL,       /**< NULL pointer argument */
    DLL_ERR_INDEX,      /**< Index out of bounds */
    DLL_ERR_EMPTY,      /**< List is empty */
    DLL_ERR_ALLOC       /**< Memory allocation failed */
} DLLResult;

/**
 * Create a new doubly linked list.
 * @return New list or NULL on allocation failure
 */
DoublyLinkedList *dll_create(void);

/**
 * Destroy a doubly linked list and free all memory.
 * @param list List to destroy (NULL safe)
 */
void dll_destroy(DoublyLinkedList *list);

/**
 * Insert a value at the front of the list.
 * @param list Target list
 * @param value Value to insert
 * @return DLL_OK on success, error code otherwise
 */
DLLResult dll_push_front(DoublyLinkedList *list, int value);

/**
 * Insert a value at the back of the list.
 * @param list Target list
 * @param value Value to insert
 * @return DLL_OK on success, error code otherwise
 */
DLLResult dll_push_back(DoublyLinkedList *list, int value);

/**
 * Remove and return the first element.
 * @param list Target list
 * @param out Pointer to store removed value (can be NULL)
 * @return DLL_OK on success, DLL_ERR_EMPTY if empty
 */
DLLResult dll_pop_front(DoublyLinkedList *list, int *out);

/**
 * Remove and return the last element.
 * @param list Target list
 * @param out Pointer to store removed value (can be NULL)
 * @return DLL_OK on success, DLL_ERR_EMPTY if empty
 */
DLLResult dll_pop_back(DoublyLinkedList *list, int *out);

/**
 * Get value at index.
 * @param list Target list
 * @param index Index to access (0-based)
 * @param out Pointer to store value
 * @return DLL_OK on success, DLL_ERR_INDEX if out of bounds
 */
DLLResult dll_get(const DoublyLinkedList *list, size_t index, int *out);

/**
 * Set value at index.
 * @param list Target list
 * @param index Index to modify (0-based)
 * @param value New value
 * @return DLL_OK on success, DLL_ERR_INDEX if out of bounds
 */
DLLResult dll_set(DoublyLinkedList *list, size_t index, int value);

/**
 * Insert value at index.
 * @param list Target list
 * @param index Index to insert at (0 to size inclusive)
 * @param value Value to insert
 * @return DLL_OK on success, error code otherwise
 */
DLLResult dll_insert(DoublyLinkedList *list, size_t index, int value);

/**
 * Remove value at index.
 * @param list Target list
 * @param index Index to remove
 * @param out Pointer to store removed value (can be NULL)
 * @return DLL_OK on success, DLL_ERR_INDEX if out of bounds
 */
DLLResult dll_remove(DoublyLinkedList *list, size_t index, int *out);

/**
 * Get the first value without removing it.
 * @param list Target list
 * @param out Pointer to store value
 * @return DLL_OK on success, DLL_ERR_EMPTY if empty
 */
DLLResult dll_front(const DoublyLinkedList *list, int *out);

/**
 * Get the last value without removing it.
 * @param list Target list
 * @param out Pointer to store value
 * @return DLL_OK on success, DLL_ERR_EMPTY if empty
 */
DLLResult dll_back(const DoublyLinkedList *list, int *out);

/**
 * Get the number of elements in the list.
 * @param list Target list
 * @return Number of elements, or 0 if NULL
 */
size_t dll_size(const DoublyLinkedList *list);

/**
 * Check if the list is empty.
 * @param list Target list
 * @return true if empty or NULL, false otherwise
 */
bool dll_is_empty(const DoublyLinkedList *list);

/**
 * Remove all elements from the list.
 * @param list Target list
 * @return DLL_OK on success, DLL_ERR_NULL if NULL
 */
DLLResult dll_clear(DoublyLinkedList *list);

/**
 * Reverse the list in place.
 * @param list Target list
 * @return DLL_OK on success, DLL_ERR_NULL if NULL
 */
DLLResult dll_reverse(DoublyLinkedList *list);

#endif /* DSA_DOUBLY_LINKED_LIST_H */
