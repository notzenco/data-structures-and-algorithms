/**
 * @file queue.c
 * @brief Queue implementation using a circular array
 */

#include "queue.h"
#include <stdlib.h>

#define DEFAULT_CAPACITY 16

/** Queue internal structure */
struct Queue {
    int *data;
    size_t head;      /* Index of front element */
    size_t tail;      /* Index of next insert position */
    size_t size;
    size_t capacity;
};

/**
 * Resize the queue's internal array.
 * @param queue Queue to resize
 * @param new_capacity New capacity
 * @return QUEUE_OK on success, QUEUE_ERR_ALLOC on failure
 */
static QueueResult queue_resize(Queue *queue, size_t new_capacity) {
    int *new_data = malloc(new_capacity * sizeof(int));
    if (!new_data) {
        return QUEUE_ERR_ALLOC;
    }

    /* Copy elements in order from head to tail */
    for (size_t i = 0; i < queue->size; i++) {
        new_data[i] = queue->data[(queue->head + i) % queue->capacity];
    }

    free(queue->data);
    queue->data = new_data;
    queue->head = 0;
    queue->tail = queue->size;
    queue->capacity = new_capacity;
    return QUEUE_OK;
}

Queue *queue_create(size_t capacity) {
    Queue *queue = malloc(sizeof(Queue));
    if (!queue) {
        return NULL;
    }

    if (capacity == 0) {
        capacity = DEFAULT_CAPACITY;
    }

    queue->data = malloc(capacity * sizeof(int));
    if (!queue->data) {
        free(queue);
        return NULL;
    }

    queue->head = 0;
    queue->tail = 0;
    queue->size = 0;
    queue->capacity = capacity;
    return queue;
}

void queue_destroy(Queue *queue) {
    if (queue) {
        free(queue->data);
        free(queue);
    }
}

QueueResult queue_enqueue(Queue *queue, int value) {
    if (!queue) {
        return QUEUE_ERR_NULL;
    }

    /* Grow if needed */
    if (queue->size == queue->capacity) {
        QueueResult result = queue_resize(queue, queue->capacity * 2);
        if (result != QUEUE_OK) {
            return result;
        }
    }

    queue->data[queue->tail] = value;
    queue->tail = (queue->tail + 1) % queue->capacity;
    queue->size++;
    return QUEUE_OK;
}

QueueResult queue_dequeue(Queue *queue, int *out) {
    if (!queue) {
        return QUEUE_ERR_NULL;
    }
    if (queue->size == 0) {
        return QUEUE_ERR_EMPTY;
    }

    if (out) {
        *out = queue->data[queue->head];
    }
    queue->head = (queue->head + 1) % queue->capacity;
    queue->size--;
    return QUEUE_OK;
}

QueueResult queue_front(const Queue *queue, int *out) {
    if (!queue) {
        return QUEUE_ERR_NULL;
    }
    if (queue->size == 0) {
        return QUEUE_ERR_EMPTY;
    }
    if (out) {
        *out = queue->data[queue->head];
    }
    return QUEUE_OK;
}

bool queue_is_empty(const Queue *queue) {
    return !queue || queue->size == 0;
}

size_t queue_size(const Queue *queue) {
    return queue ? queue->size : 0;
}

QueueResult queue_clear(Queue *queue) {
    if (!queue) {
        return QUEUE_ERR_NULL;
    }
    queue->head = 0;
    queue->tail = 0;
    queue->size = 0;
    return QUEUE_OK;
}
