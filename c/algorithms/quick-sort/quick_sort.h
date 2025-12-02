/**
 * @file quick_sort.h
 * @brief Quick sort algorithm interface
 *
 * Quick sort is a divide-and-conquer algorithm that picks a pivot element
 * and partitions the array around it.
 * Time complexity: O(n log n) average, O(n^2) worst case.
 * Space complexity: O(log n) for the recursion stack.
 */

#ifndef DSA_QUICK_SORT_H
#define DSA_QUICK_SORT_H

#include <stddef.h>

/**
 * Sort an array in ascending order using quick sort.
 * @param arr Array to sort
 * @param size Number of elements in array
 */
void quick_sort(int *arr, size_t size);

/**
 * Sort an array in descending order using quick sort.
 * @param arr Array to sort
 * @param size Number of elements in array
 */
void quick_sort_desc(int *arr, size_t size);

#endif /* DSA_QUICK_SORT_H */
