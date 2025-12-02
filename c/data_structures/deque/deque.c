/**
 * @file deque.c
 * @brief Deque implementation using a circular array
 */

#include "deque.h"
#include <stdlib.h>

#define DEFAULT_CAPACITY 16

/** Deque internal structure */
struct Deque {
    int *data;
    size_t head;      /* Index of front element */
    size_t size;
    size_t capacity;
};

/**
 * Calculate actual array index from logical index.
 * @param deque Target deque
 * @param index Logical index (0 = front)
 * @return Actual array index
 */
static size_t get_actual_index(const Deque *deque, size_t index) {
    return (deque->head + index) % deque->capacity;
}

/**
 * Resize the deque's internal array.
 * @param deque Deque to resize
 * @param new_capacity New capacity
 * @return DEQUE_OK on success, DEQUE_ERR_ALLOC on failure
 */
static DequeResult deque_resize(Deque *deque, size_t new_capacity) {
    int *new_data = malloc(new_capacity * sizeof(int));
    if (!new_data) {
        return DEQUE_ERR_ALLOC;
    }

    /* Copy elements in order from front to back */
    for (size_t i = 0; i < deque->size; i++) {
        new_data[i] = deque->data[get_actual_index(deque, i)];
    }

    free(deque->data);
    deque->data = new_data;
    deque->head = 0;
    deque->capacity = new_capacity;
    return DEQUE_OK;
}

Deque *deque_create(size_t capacity) {
    Deque *deque = malloc(sizeof(Deque));
    if (!deque) {
        return NULL;
    }

    if (capacity == 0) {
        capacity = DEFAULT_CAPACITY;
    }

    deque->data = malloc(capacity * sizeof(int));
    if (!deque->data) {
        free(deque);
        return NULL;
    }

    deque->head = 0;
    deque->size = 0;
    deque->capacity = capacity;
    return deque;
}

void deque_destroy(Deque *deque) {
    if (deque) {
        free(deque->data);
        free(deque);
    }
}

DequeResult deque_push_front(Deque *deque, int value) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }

    /* Grow if needed */
    if (deque->size == deque->capacity) {
        DequeResult result = deque_resize(deque, deque->capacity * 2);
        if (result != DEQUE_OK) {
            return result;
        }
    }

    /* Move head back (with wrap-around) */
    if (deque->head == 0) {
        deque->head = deque->capacity - 1;
    } else {
        deque->head--;
    }

    deque->data[deque->head] = value;
    deque->size++;
    return DEQUE_OK;
}

DequeResult deque_push_back(Deque *deque, int value) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }

    /* Grow if needed */
    if (deque->size == deque->capacity) {
        DequeResult result = deque_resize(deque, deque->capacity * 2);
        if (result != DEQUE_OK) {
            return result;
        }
    }

    size_t tail = get_actual_index(deque, deque->size);
    deque->data[tail] = value;
    deque->size++;
    return DEQUE_OK;
}

DequeResult deque_pop_front(Deque *deque, int *out) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }
    if (deque->size == 0) {
        return DEQUE_ERR_EMPTY;
    }

    if (out) {
        *out = deque->data[deque->head];
    }

    deque->head = (deque->head + 1) % deque->capacity;
    deque->size--;
    return DEQUE_OK;
}

DequeResult deque_pop_back(Deque *deque, int *out) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }
    if (deque->size == 0) {
        return DEQUE_ERR_EMPTY;
    }

    deque->size--;
    if (out) {
        *out = deque->data[get_actual_index(deque, deque->size)];
    }
    return DEQUE_OK;
}

DequeResult deque_front(const Deque *deque, int *out) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }
    if (deque->size == 0) {
        return DEQUE_ERR_EMPTY;
    }
    if (out) {
        *out = deque->data[deque->head];
    }
    return DEQUE_OK;
}

DequeResult deque_back(const Deque *deque, int *out) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }
    if (deque->size == 0) {
        return DEQUE_ERR_EMPTY;
    }
    if (out) {
        *out = deque->data[get_actual_index(deque, deque->size - 1)];
    }
    return DEQUE_OK;
}

DequeResult deque_get(const Deque *deque, size_t index, int *out) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }
    if (index >= deque->size) {
        return DEQUE_ERR_INDEX;
    }
    if (out) {
        *out = deque->data[get_actual_index(deque, index)];
    }
    return DEQUE_OK;
}

DequeResult deque_set(Deque *deque, size_t index, int value) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }
    if (index >= deque->size) {
        return DEQUE_ERR_INDEX;
    }

    deque->data[get_actual_index(deque, index)] = value;
    return DEQUE_OK;
}

size_t deque_size(const Deque *deque) {
    return deque ? deque->size : 0;
}

bool deque_is_empty(const Deque *deque) {
    return !deque || deque->size == 0;
}

DequeResult deque_clear(Deque *deque) {
    if (!deque) {
        return DEQUE_ERR_NULL;
    }
    deque->head = 0;
    deque->size = 0;
    return DEQUE_OK;
}
