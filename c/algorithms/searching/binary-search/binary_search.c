/**
 * @file binary_search.c
 * @brief Binary search algorithm implementation
 */

#include "binary_search.h"

int binary_search(const int *arr, size_t size, int target) {
    if (!arr || size == 0) {
        return -1;
    }

    size_t left = 0;
    size_t right = size - 1;

    while (left <= right) {
        size_t mid = left + (right - left) / 2;

        if (arr[mid] == target) {
            return (int)mid;
        } else if (arr[mid] < target) {
            left = mid + 1;
        } else {
            if (mid == 0) break;
            right = mid - 1;
        }
    }

    return -1;
}

/**
 * Recursive helper for binary search.
 */
static int binary_search_recursive_helper(const int *arr, size_t left, size_t right, int target) {
    if (left > right) {
        return -1;
    }

    size_t mid = left + (right - left) / 2;

    if (arr[mid] == target) {
        return (int)mid;
    } else if (arr[mid] < target) {
        return binary_search_recursive_helper(arr, mid + 1, right, target);
    } else {
        if (mid == 0) return -1;
        return binary_search_recursive_helper(arr, left, mid - 1, target);
    }
}

int binary_search_recursive(const int *arr, size_t size, int target) {
    if (!arr || size == 0) {
        return -1;
    }
    return binary_search_recursive_helper(arr, 0, size - 1, target);
}

int binary_search_left(const int *arr, size_t size, int target) {
    if (!arr || size == 0) {
        return -1;
    }

    size_t left = 0;
    size_t right = size;

    while (left < right) {
        size_t mid = left + (right - left) / 2;

        if (arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    if (left < size && arr[left] == target) {
        return (int)left;
    }
    return -1;
}

int binary_search_right(const int *arr, size_t size, int target) {
    if (!arr || size == 0) {
        return -1;
    }

    size_t left = 0;
    size_t right = size;

    while (left < right) {
        size_t mid = left + (right - left) / 2;

        if (arr[mid] <= target) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }

    if (left > 0 && arr[left - 1] == target) {
        return (int)(left - 1);
    }
    return -1;
}
