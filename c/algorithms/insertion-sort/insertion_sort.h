/**
 * @file insertion_sort.h
 * @brief Insertion sort algorithm interface
 *
 * Insertion sort builds the sorted array one element at a time by
 * repeatedly picking the next element and inserting it into its correct position.
 * Time complexity: O(n^2) worst/average, O(n) best (already sorted).
 * Space complexity: O(1) - in-place sorting.
 */

#ifndef DSA_INSERTION_SORT_H
#define DSA_INSERTION_SORT_H

#include <stddef.h>

/**
 * Sort an array in ascending order using insertion sort.
 * @param arr Array to sort
 * @param size Number of elements in array
 */
void insertion_sort(int *arr, size_t size);

/**
 * Sort an array in descending order using insertion sort.
 * @param arr Array to sort
 * @param size Number of elements in array
 */
void insertion_sort_desc(int *arr, size_t size);

#endif /* DSA_INSERTION_SORT_H */
