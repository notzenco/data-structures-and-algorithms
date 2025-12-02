/**
 * @file merge_sort.c
 * @brief Merge sort algorithm implementation
 */

#include "merge_sort.h"
#include <stdlib.h>
#include <string.h>

/**
 * Merge two sorted subarrays into one sorted array (ascending).
 * @param arr Original array
 * @param temp Temporary array for merging
 * @param left Left index
 * @param mid Middle index
 * @param right Right index
 */
static void merge_asc(int *arr, int *temp, size_t left, size_t mid, size_t right) {
    size_t i = left;
    size_t j = mid + 1;
    size_t k = left;

    while (i <= mid && j <= right) {
        if (arr[i] <= arr[j]) {
            temp[k++] = arr[i++];
        } else {
            temp[k++] = arr[j++];
        }
    }

    while (i <= mid) {
        temp[k++] = arr[i++];
    }

    while (j <= right) {
        temp[k++] = arr[j++];
    }

    memcpy(&arr[left], &temp[left], (right - left + 1) * sizeof(int));
}

/**
 * Merge two sorted subarrays into one sorted array (descending).
 */
static void merge_desc(int *arr, int *temp, size_t left, size_t mid, size_t right) {
    size_t i = left;
    size_t j = mid + 1;
    size_t k = left;

    while (i <= mid && j <= right) {
        if (arr[i] >= arr[j]) {
            temp[k++] = arr[i++];
        } else {
            temp[k++] = arr[j++];
        }
    }

    while (i <= mid) {
        temp[k++] = arr[i++];
    }

    while (j <= right) {
        temp[k++] = arr[j++];
    }

    memcpy(&arr[left], &temp[left], (right - left + 1) * sizeof(int));
}

/**
 * Recursive merge sort helper (ascending).
 */
static void merge_sort_recursive_asc(int *arr, int *temp, size_t left, size_t right) {
    if (left >= right) {
        return;
    }

    size_t mid = left + (right - left) / 2;

    merge_sort_recursive_asc(arr, temp, left, mid);
    merge_sort_recursive_asc(arr, temp, mid + 1, right);
    merge_asc(arr, temp, left, mid, right);
}

/**
 * Recursive merge sort helper (descending).
 */
static void merge_sort_recursive_desc(int *arr, int *temp, size_t left, size_t right) {
    if (left >= right) {
        return;
    }

    size_t mid = left + (right - left) / 2;

    merge_sort_recursive_desc(arr, temp, left, mid);
    merge_sort_recursive_desc(arr, temp, mid + 1, right);
    merge_desc(arr, temp, left, mid, right);
}

void merge_sort(int *arr, size_t size) {
    if (!arr || size < 2) {
        return;
    }

    int *temp = malloc(size * sizeof(int));
    if (!temp) {
        return;  /* Allocation failed */
    }

    merge_sort_recursive_asc(arr, temp, 0, size - 1);

    free(temp);
}

void merge_sort_desc(int *arr, size_t size) {
    if (!arr || size < 2) {
        return;
    }

    int *temp = malloc(size * sizeof(int));
    if (!temp) {
        return;
    }

    merge_sort_recursive_desc(arr, temp, 0, size - 1);

    free(temp);
}
