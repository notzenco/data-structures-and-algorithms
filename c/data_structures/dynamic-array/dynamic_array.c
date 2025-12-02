/**
 * @file dynamic_array.c
 * @brief Dynamic array implementation
 */

#include "dynamic_array.h"
#include <stdlib.h>
#include <string.h>

#define DEFAULT_CAPACITY 16
#define GROWTH_FACTOR 2

/** Dynamic array internal structure */
struct DynamicArray {
    int *data;
    size_t size;
    size_t capacity;
};

/**
 * Ensure the array has at least the specified capacity.
 * @param array Target array
 * @param min_capacity Minimum required capacity
 * @return DA_OK on success, DA_ERR_ALLOC on failure
 */
static DAResult da_ensure_capacity(DynamicArray *array, size_t min_capacity) {
    if (array->capacity >= min_capacity) {
        return DA_OK;
    }

    size_t new_capacity = array->capacity;
    while (new_capacity < min_capacity) {
        new_capacity *= GROWTH_FACTOR;
    }

    int *new_data = realloc(array->data, new_capacity * sizeof(int));
    if (!new_data) {
        return DA_ERR_ALLOC;
    }

    array->data = new_data;
    array->capacity = new_capacity;
    return DA_OK;
}

DynamicArray *da_create(size_t capacity) {
    DynamicArray *array = malloc(sizeof(DynamicArray));
    if (!array) {
        return NULL;
    }

    if (capacity == 0) {
        capacity = DEFAULT_CAPACITY;
    }

    array->data = malloc(capacity * sizeof(int));
    if (!array->data) {
        free(array);
        return NULL;
    }

    array->size = 0;
    array->capacity = capacity;
    return array;
}

void da_destroy(DynamicArray *array) {
    if (array) {
        free(array->data);
        free(array);
    }
}

DAResult da_push(DynamicArray *array, int value) {
    if (!array) {
        return DA_ERR_NULL;
    }

    DAResult result = da_ensure_capacity(array, array->size + 1);
    if (result != DA_OK) {
        return result;
    }

    array->data[array->size++] = value;
    return DA_OK;
}

DAResult da_pop(DynamicArray *array, int *out) {
    if (!array) {
        return DA_ERR_NULL;
    }
    if (array->size == 0) {
        return DA_ERR_EMPTY;
    }

    array->size--;
    if (out) {
        *out = array->data[array->size];
    }
    return DA_OK;
}

DAResult da_get(const DynamicArray *array, size_t index, int *out) {
    if (!array) {
        return DA_ERR_NULL;
    }
    if (index >= array->size) {
        return DA_ERR_INDEX;
    }
    if (out) {
        *out = array->data[index];
    }
    return DA_OK;
}

DAResult da_set(DynamicArray *array, size_t index, int value) {
    if (!array) {
        return DA_ERR_NULL;
    }
    if (index >= array->size) {
        return DA_ERR_INDEX;
    }

    array->data[index] = value;
    return DA_OK;
}

DAResult da_insert(DynamicArray *array, size_t index, int value) {
    if (!array) {
        return DA_ERR_NULL;
    }
    if (index > array->size) {
        return DA_ERR_INDEX;
    }

    DAResult result = da_ensure_capacity(array, array->size + 1);
    if (result != DA_OK) {
        return result;
    }

    /* Shift elements right */
    if (index < array->size) {
        memmove(&array->data[index + 1], &array->data[index],
                (array->size - index) * sizeof(int));
    }

    array->data[index] = value;
    array->size++;
    return DA_OK;
}

DAResult da_remove(DynamicArray *array, size_t index, int *out) {
    if (!array) {
        return DA_ERR_NULL;
    }
    if (index >= array->size) {
        return DA_ERR_INDEX;
    }

    if (out) {
        *out = array->data[index];
    }

    /* Shift elements left */
    if (index < array->size - 1) {
        memmove(&array->data[index], &array->data[index + 1],
                (array->size - index - 1) * sizeof(int));
    }

    array->size--;
    return DA_OK;
}

size_t da_size(const DynamicArray *array) {
    return array ? array->size : 0;
}

size_t da_capacity(const DynamicArray *array) {
    return array ? array->capacity : 0;
}

bool da_is_empty(const DynamicArray *array) {
    return !array || array->size == 0;
}

DAResult da_clear(DynamicArray *array) {
    if (!array) {
        return DA_ERR_NULL;
    }
    array->size = 0;
    return DA_OK;
}

DAResult da_shrink_to_fit(DynamicArray *array) {
    if (!array) {
        return DA_ERR_NULL;
    }
    if (array->size == 0) {
        return DA_OK;
    }
    if (array->size == array->capacity) {
        return DA_OK;
    }

    int *new_data = realloc(array->data, array->size * sizeof(int));
    if (!new_data) {
        return DA_ERR_ALLOC;
    }

    array->data = new_data;
    array->capacity = array->size;
    return DA_OK;
}
