/**
 * @file heap.c
 * @brief Binary Min Heap implementation
 */

#include "heap.h"
#include <stdlib.h>

#define DEFAULT_CAPACITY 16

/** Heap internal structure */
struct Heap {
    int *data;
    size_t size;
    size_t capacity;
};

/**
 * Get parent index.
 */
static size_t parent(size_t i) {
    return (i - 1) / 2;
}

/**
 * Get left child index.
 */
static size_t left_child(size_t i) {
    return 2 * i + 1;
}

/**
 * Get right child index.
 */
static size_t right_child(size_t i) {
    return 2 * i + 2;
}

/**
 * Swap two elements.
 */
static void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

/**
 * Bubble up to restore heap property after insertion.
 * @param heap Target heap
 * @param index Index to start bubbling from
 */
static void sift_up(Heap *heap, size_t index) {
    while (index > 0 && heap->data[parent(index)] > heap->data[index]) {
        swap(&heap->data[parent(index)], &heap->data[index]);
        index = parent(index);
    }
}

/**
 * Bubble down to restore heap property after removal.
 * @param heap Target heap
 * @param index Index to start bubbling from
 */
static void sift_down(Heap *heap, size_t index) {
    size_t smallest = index;

    while (true) {
        size_t left = left_child(index);
        size_t right = right_child(index);

        if (left < heap->size && heap->data[left] < heap->data[smallest]) {
            smallest = left;
        }
        if (right < heap->size && heap->data[right] < heap->data[smallest]) {
            smallest = right;
        }

        if (smallest == index) {
            break;
        }

        swap(&heap->data[index], &heap->data[smallest]);
        index = smallest;
    }
}

/**
 * Ensure the heap has at least the specified capacity.
 * @param heap Target heap
 * @param min_capacity Minimum required capacity
 * @return HEAP_OK on success, HEAP_ERR_ALLOC on failure
 */
static HeapResult heap_ensure_capacity(Heap *heap, size_t min_capacity) {
    if (heap->capacity >= min_capacity) {
        return HEAP_OK;
    }

    size_t new_capacity = heap->capacity;
    while (new_capacity < min_capacity) {
        new_capacity *= 2;
    }

    int *new_data = realloc(heap->data, new_capacity * sizeof(int));
    if (!new_data) {
        return HEAP_ERR_ALLOC;
    }

    heap->data = new_data;
    heap->capacity = new_capacity;
    return HEAP_OK;
}

Heap *heap_create(size_t capacity) {
    Heap *heap = malloc(sizeof(Heap));
    if (!heap) {
        return NULL;
    }

    if (capacity == 0) {
        capacity = DEFAULT_CAPACITY;
    }

    heap->data = malloc(capacity * sizeof(int));
    if (!heap->data) {
        free(heap);
        return NULL;
    }

    heap->size = 0;
    heap->capacity = capacity;
    return heap;
}

void heap_destroy(Heap *heap) {
    if (heap) {
        free(heap->data);
        free(heap);
    }
}

HeapResult heap_push(Heap *heap, int value) {
    if (!heap) {
        return HEAP_ERR_NULL;
    }

    HeapResult result = heap_ensure_capacity(heap, heap->size + 1);
    if (result != HEAP_OK) {
        return result;
    }

    /* Insert at end */
    heap->data[heap->size] = value;
    heap->size++;

    /* Restore heap property */
    sift_up(heap, heap->size - 1);

    return HEAP_OK;
}

HeapResult heap_pop(Heap *heap, int *out) {
    if (!heap) {
        return HEAP_ERR_NULL;
    }
    if (heap->size == 0) {
        return HEAP_ERR_EMPTY;
    }

    if (out) {
        *out = heap->data[0];
    }

    /* Move last element to root */
    heap->size--;
    if (heap->size > 0) {
        heap->data[0] = heap->data[heap->size];
        sift_down(heap, 0);
    }

    return HEAP_OK;
}

HeapResult heap_peek(const Heap *heap, int *out) {
    if (!heap) {
        return HEAP_ERR_NULL;
    }
    if (heap->size == 0) {
        return HEAP_ERR_EMPTY;
    }
    if (out) {
        *out = heap->data[0];
    }
    return HEAP_OK;
}

size_t heap_size(const Heap *heap) {
    return heap ? heap->size : 0;
}

bool heap_is_empty(const Heap *heap) {
    return !heap || heap->size == 0;
}

HeapResult heap_clear(Heap *heap) {
    if (!heap) {
        return HEAP_ERR_NULL;
    }
    heap->size = 0;
    return HEAP_OK;
}

Heap *heap_from_array(const int *values, size_t count) {
    if (!values && count > 0) {
        return NULL;
    }

    Heap *heap = heap_create(count > 0 ? count : DEFAULT_CAPACITY);
    if (!heap) {
        return NULL;
    }

    /* Copy values */
    for (size_t i = 0; i < count; i++) {
        heap->data[i] = values[i];
    }
    heap->size = count;

    /* Heapify: start from last non-leaf and sift down */
    if (count > 1) {
        for (size_t i = count / 2; i > 0; i--) {
            sift_down(heap, i - 1);
        }
        sift_down(heap, 0);
    }

    return heap;
}
