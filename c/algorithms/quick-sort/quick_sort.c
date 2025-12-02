/**
 * @file quick_sort.c
 * @brief Quick sort algorithm implementation
 */

#include "quick_sort.h"

/**
 * Swap two elements.
 */
static void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

/**
 * Partition array around pivot (ascending order).
 * Uses Lomuto partition scheme with last element as pivot.
 * @param arr Array to partition
 * @param low Low index
 * @param high High index (pivot)
 * @return Final position of pivot
 */
static size_t partition_asc(int *arr, size_t low, size_t high) {
    int pivot = arr[high];
    size_t i = low;

    for (size_t j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            swap(&arr[i], &arr[j]);
            i++;
        }
    }

    swap(&arr[i], &arr[high]);
    return i;
}

/**
 * Partition array around pivot (descending order).
 */
static size_t partition_desc(int *arr, size_t low, size_t high) {
    int pivot = arr[high];
    size_t i = low;

    for (size_t j = low; j < high; j++) {
        if (arr[j] >= pivot) {
            swap(&arr[i], &arr[j]);
            i++;
        }
    }

    swap(&arr[i], &arr[high]);
    return i;
}

/**
 * Recursive quick sort helper (ascending).
 */
static void quick_sort_recursive_asc(int *arr, size_t low, size_t high) {
    if (low >= high) {
        return;
    }

    size_t pivot_idx = partition_asc(arr, low, high);

    if (pivot_idx > 0) {
        quick_sort_recursive_asc(arr, low, pivot_idx - 1);
    }
    quick_sort_recursive_asc(arr, pivot_idx + 1, high);
}

/**
 * Recursive quick sort helper (descending).
 */
static void quick_sort_recursive_desc(int *arr, size_t low, size_t high) {
    if (low >= high) {
        return;
    }

    size_t pivot_idx = partition_desc(arr, low, high);

    if (pivot_idx > 0) {
        quick_sort_recursive_desc(arr, low, pivot_idx - 1);
    }
    quick_sort_recursive_desc(arr, pivot_idx + 1, high);
}

void quick_sort(int *arr, size_t size) {
    if (!arr || size < 2) {
        return;
    }

    quick_sort_recursive_asc(arr, 0, size - 1);
}

void quick_sort_desc(int *arr, size_t size) {
    if (!arr || size < 2) {
        return;
    }

    quick_sort_recursive_desc(arr, 0, size - 1);
}
