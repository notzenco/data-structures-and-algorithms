/**
 * @file queue.h
 * @brief Queue data structure (FIFO) - circular array implementation
 *
 * A queue is a linear data structure that follows the First-In-First-Out (FIFO)
 * principle. Elements are added at the rear and removed from the front.
 *
 * Time Complexity:
 *   - enqueue: O(1) amortized
 *   - dequeue: O(1)
 *   - front:   O(1)
 *   - isEmpty: O(1)
 *   - size:    O(1)
 *
 * Space Complexity: O(n) where n is the number of elements
 */

#ifndef DSA_QUEUE_H
#define DSA_QUEUE_H

#include <stdbool.h>
#include <stddef.h>

/** Opaque queue type */
typedef struct Queue Queue;

/** Result codes for queue operations */
typedef enum {
    QUEUE_OK = 0,
    QUEUE_ERR_NULL,      /* NULL pointer argument */
    QUEUE_ERR_EMPTY,     /* Operation on empty queue */
    QUEUE_ERR_ALLOC      /* Memory allocation failed */
} QueueResult;

/**
 * Create a new queue with specified initial capacity.
 *
 * @param capacity Initial capacity (0 for default)
 * @return Pointer to new queue, or NULL on allocation failure
 */
Queue *queue_create(size_t capacity);

/**
 * Destroy queue and free all memory.
 *
 * @param queue Queue to destroy (NULL safe)
 */
void queue_destroy(Queue *queue);

/**
 * Add an element to the rear of the queue.
 *
 * @param queue Target queue
 * @param value Value to enqueue
 * @return QUEUE_OK on success, error code otherwise
 */
QueueResult queue_enqueue(Queue *queue, int value);

/**
 * Remove an element from the front of the queue.
 *
 * @param queue Target queue
 * @param out   Pointer to store dequeued value (can be NULL to discard)
 * @return QUEUE_OK on success, QUEUE_ERR_EMPTY if queue is empty
 */
QueueResult queue_dequeue(Queue *queue, int *out);

/**
 * Peek at the front element without removing it.
 *
 * @param queue Target queue
 * @param out   Pointer to store front value
 * @return QUEUE_OK on success, QUEUE_ERR_EMPTY if queue is empty
 */
QueueResult queue_front(const Queue *queue, int *out);

/**
 * Check if queue is empty.
 *
 * @param queue Target queue
 * @return true if empty or NULL, false otherwise
 */
bool queue_is_empty(const Queue *queue);

/**
 * Get the number of elements in the queue.
 *
 * @param queue Target queue
 * @return Number of elements, 0 if NULL
 */
size_t queue_size(const Queue *queue);

/**
 * Remove all elements from the queue.
 *
 * @param queue Target queue
 * @return QUEUE_OK on success, QUEUE_ERR_NULL if queue is NULL
 */
QueueResult queue_clear(Queue *queue);

#endif /* DSA_QUEUE_H */
