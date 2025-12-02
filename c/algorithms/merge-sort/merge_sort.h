/**
 * @file merge_sort.h
 * @brief Merge sort algorithm interface
 *
 * Merge sort is a divide-and-conquer algorithm that divides the array into halves,
 * recursively sorts them, and merges the sorted halves.
 * Time complexity: O(n log n) for all cases.
 * Space complexity: O(n) for the temporary array.
 */

#ifndef DSA_MERGE_SORT_H
#define DSA_MERGE_SORT_H

#include <stddef.h>

/**
 * Sort an array in ascending order using merge sort.
 * @param arr Array to sort
 * @param size Number of elements in array
 */
void merge_sort(int *arr, size_t size);

/**
 * Sort an array in descending order using merge sort.
 * @param arr Array to sort
 * @param size Number of elements in array
 */
void merge_sort_desc(int *arr, size_t size);

#endif /* DSA_MERGE_SORT_H */
