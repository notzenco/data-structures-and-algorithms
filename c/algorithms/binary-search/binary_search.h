/**
 * @file binary_search.h
 * @brief Binary search algorithm interface
 *
 * Binary search finds a target value in a sorted array by repeatedly
 * dividing the search interval in half. Time complexity: O(log n).
 */

#ifndef DSA_BINARY_SEARCH_H
#define DSA_BINARY_SEARCH_H

#include <stddef.h>

/**
 * Search for a target value in a sorted array.
 * @param arr Sorted array to search
 * @param size Number of elements in array
 * @param target Value to find
 * @return Index of target if found, -1 otherwise
 */
int binary_search(const int *arr, size_t size, int target);

/**
 * Search for a target value in a sorted array (recursive version).
 * @param arr Sorted array to search
 * @param size Number of elements in array
 * @param target Value to find
 * @return Index of target if found, -1 otherwise
 */
int binary_search_recursive(const int *arr, size_t size, int target);

/**
 * Find the leftmost (first) occurrence of target in a sorted array.
 * @param arr Sorted array to search
 * @param size Number of elements in array
 * @param target Value to find
 * @return Index of first occurrence, -1 if not found
 */
int binary_search_left(const int *arr, size_t size, int target);

/**
 * Find the rightmost (last) occurrence of target in a sorted array.
 * @param arr Sorted array to search
 * @param size Number of elements in array
 * @param target Value to find
 * @return Index of last occurrence, -1 if not found
 */
int binary_search_right(const int *arr, size_t size, int target);

#endif /* DSA_BINARY_SEARCH_H */
