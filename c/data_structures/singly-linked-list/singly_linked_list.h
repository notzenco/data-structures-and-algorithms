/**
 * @file singly_linked_list.h
 * @brief Singly linked list interface
 *
 * A linked list where each node contains a value and a pointer to the next node.
 * Provides O(1) insertion/deletion at head and O(n) access by index.
 */

#ifndef DSA_SINGLY_LINKED_LIST_H
#define DSA_SINGLY_LINKED_LIST_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque singly linked list type */
typedef struct SinglyLinkedList SinglyLinkedList;

/** Result codes for singly linked list operations */
typedef enum {
    SLL_OK = 0,         /**< Operation successful */
    SLL_ERR_NULL,       /**< NULL pointer argument */
    SLL_ERR_INDEX,      /**< Index out of bounds */
    SLL_ERR_EMPTY,      /**< List is empty */
    SLL_ERR_ALLOC       /**< Memory allocation failed */
} SLLResult;

/**
 * Create a new singly linked list.
 * @return New list or NULL on allocation failure
 */
SinglyLinkedList *sll_create(void);

/**
 * Destroy a singly linked list and free all memory.
 * @param list List to destroy (NULL safe)
 */
void sll_destroy(SinglyLinkedList *list);

/**
 * Insert a value at the front of the list.
 * @param list Target list
 * @param value Value to insert
 * @return SLL_OK on success, error code otherwise
 */
SLLResult sll_push_front(SinglyLinkedList *list, int value);

/**
 * Insert a value at the back of the list.
 * @param list Target list
 * @param value Value to insert
 * @return SLL_OK on success, error code otherwise
 */
SLLResult sll_push_back(SinglyLinkedList *list, int value);

/**
 * Remove and return the first element.
 * @param list Target list
 * @param out Pointer to store removed value (can be NULL)
 * @return SLL_OK on success, SLL_ERR_EMPTY if empty
 */
SLLResult sll_pop_front(SinglyLinkedList *list, int *out);

/**
 * Get value at index.
 * @param list Target list
 * @param index Index to access (0-based)
 * @param out Pointer to store value
 * @return SLL_OK on success, SLL_ERR_INDEX if out of bounds
 */
SLLResult sll_get(const SinglyLinkedList *list, size_t index, int *out);

/**
 * Set value at index.
 * @param list Target list
 * @param index Index to modify (0-based)
 * @param value New value
 * @return SLL_OK on success, SLL_ERR_INDEX if out of bounds
 */
SLLResult sll_set(SinglyLinkedList *list, size_t index, int value);

/**
 * Insert value at index.
 * @param list Target list
 * @param index Index to insert at (0 to size inclusive)
 * @param value Value to insert
 * @return SLL_OK on success, error code otherwise
 */
SLLResult sll_insert(SinglyLinkedList *list, size_t index, int value);

/**
 * Remove value at index.
 * @param list Target list
 * @param index Index to remove
 * @param out Pointer to store removed value (can be NULL)
 * @return SLL_OK on success, SLL_ERR_INDEX if out of bounds
 */
SLLResult sll_remove(SinglyLinkedList *list, size_t index, int *out);

/**
 * Get the first value without removing it.
 * @param list Target list
 * @param out Pointer to store value
 * @return SLL_OK on success, SLL_ERR_EMPTY if empty
 */
SLLResult sll_front(const SinglyLinkedList *list, int *out);

/**
 * Get the number of elements in the list.
 * @param list Target list
 * @return Number of elements, or 0 if NULL
 */
size_t sll_size(const SinglyLinkedList *list);

/**
 * Check if the list is empty.
 * @param list Target list
 * @return true if empty or NULL, false otherwise
 */
bool sll_is_empty(const SinglyLinkedList *list);

/**
 * Remove all elements from the list.
 * @param list Target list
 * @return SLL_OK on success, SLL_ERR_NULL if NULL
 */
SLLResult sll_clear(SinglyLinkedList *list);

/**
 * Reverse the list in place.
 * @param list Target list
 * @return SLL_OK on success, SLL_ERR_NULL if NULL
 */
SLLResult sll_reverse(SinglyLinkedList *list);

#endif /* DSA_SINGLY_LINKED_LIST_H */
