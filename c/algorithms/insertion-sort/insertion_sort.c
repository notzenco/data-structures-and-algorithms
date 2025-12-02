/**
 * @file insertion_sort.c
 * @brief Insertion sort algorithm implementation
 */

#include "insertion_sort.h"

void insertion_sort(int *arr, size_t size) {
    if (!arr || size < 2) {
        return;
    }

    for (size_t i = 1; i < size; i++) {
        int key = arr[i];
        size_t j = i;

        /* Shift elements greater than key to the right */
        while (j > 0 && arr[j - 1] > key) {
            arr[j] = arr[j - 1];
            j--;
        }

        arr[j] = key;
    }
}

void insertion_sort_desc(int *arr, size_t size) {
    if (!arr || size < 2) {
        return;
    }

    for (size_t i = 1; i < size; i++) {
        int key = arr[i];
        size_t j = i;

        /* Shift elements smaller than key to the right */
        while (j > 0 && arr[j - 1] < key) {
            arr[j] = arr[j - 1];
            j--;
        }

        arr[j] = key;
    }
}
