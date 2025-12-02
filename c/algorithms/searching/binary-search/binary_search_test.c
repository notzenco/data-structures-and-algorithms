/**
 * @file binary_search_test.c
 * @brief Unit tests for binary search implementation
 */

#include "binary_search.h"
#include <assert.h>
#include <stdio.h>

static void test_basic_search(void) {
    printf("  test_basic_search...");

    int arr[] = {1, 3, 5, 7, 9, 11, 13, 15};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    assert(binary_search(arr, size, 1) == 0);
    assert(binary_search(arr, size, 7) == 3);
    assert(binary_search(arr, size, 15) == 7);
    assert(binary_search(arr, size, 9) == 4);

    printf(" PASSED\n");
}

static void test_not_found(void) {
    printf("  test_not_found...");

    int arr[] = {1, 3, 5, 7, 9};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    assert(binary_search(arr, size, 0) == -1);
    assert(binary_search(arr, size, 2) == -1);
    assert(binary_search(arr, size, 10) == -1);
    assert(binary_search(arr, size, -5) == -1);

    printf(" PASSED\n");
}

static void test_single_element(void) {
    printf("  test_single_element...");

    int arr[] = {42};

    assert(binary_search(arr, 1, 42) == 0);
    assert(binary_search(arr, 1, 0) == -1);
    assert(binary_search(arr, 1, 100) == -1);

    printf(" PASSED\n");
}

static void test_empty_array(void) {
    printf("  test_empty_array...");

    int arr[] = {1};

    assert(binary_search(arr, 0, 1) == -1);
    assert(binary_search(NULL, 0, 1) == -1);
    assert(binary_search(NULL, 5, 1) == -1);

    printf(" PASSED\n");
}

static void test_recursive(void) {
    printf("  test_recursive...");

    int arr[] = {2, 4, 6, 8, 10, 12, 14, 16, 18, 20};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    assert(binary_search_recursive(arr, size, 2) == 0);
    assert(binary_search_recursive(arr, size, 10) == 4);
    assert(binary_search_recursive(arr, size, 20) == 9);
    assert(binary_search_recursive(arr, size, 5) == -1);
    assert(binary_search_recursive(arr, size, 21) == -1);

    printf(" PASSED\n");
}

static void test_left_boundary(void) {
    printf("  test_left_boundary...");

    int arr[] = {1, 2, 2, 2, 3, 4, 4, 5};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    /* Find first occurrence */
    assert(binary_search_left(arr, size, 2) == 1);
    assert(binary_search_left(arr, size, 4) == 5);
    assert(binary_search_left(arr, size, 1) == 0);
    assert(binary_search_left(arr, size, 5) == 7);

    /* Not found */
    assert(binary_search_left(arr, size, 0) == -1);
    assert(binary_search_left(arr, size, 6) == -1);

    printf(" PASSED\n");
}

static void test_right_boundary(void) {
    printf("  test_right_boundary...");

    int arr[] = {1, 2, 2, 2, 3, 4, 4, 5};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    /* Find last occurrence */
    assert(binary_search_right(arr, size, 2) == 3);
    assert(binary_search_right(arr, size, 4) == 6);
    assert(binary_search_right(arr, size, 1) == 0);
    assert(binary_search_right(arr, size, 5) == 7);

    /* Not found */
    assert(binary_search_right(arr, size, 0) == -1);
    assert(binary_search_right(arr, size, 6) == -1);

    printf(" PASSED\n");
}

static void test_all_same_elements(void) {
    printf("  test_all_same_elements...");

    int arr[] = {5, 5, 5, 5, 5};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    assert(binary_search(arr, size, 5) >= 0);
    assert(binary_search_left(arr, size, 5) == 0);
    assert(binary_search_right(arr, size, 5) == 4);

    printf(" PASSED\n");
}

static void test_negative_numbers(void) {
    printf("  test_negative_numbers...");

    int arr[] = {-10, -5, -3, 0, 2, 7, 15};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    assert(binary_search(arr, size, -10) == 0);
    assert(binary_search(arr, size, -5) == 1);
    assert(binary_search(arr, size, 0) == 3);
    assert(binary_search(arr, size, 15) == 6);
    assert(binary_search(arr, size, -7) == -1);

    printf(" PASSED\n");
}

static void test_large_array(void) {
    printf("  test_large_array...");

    int arr[1000];
    for (int i = 0; i < 1000; i++) {
        arr[i] = i * 2;  /* Even numbers: 0, 2, 4, ..., 1998 */
    }

    assert(binary_search(arr, 1000, 0) == 0);
    assert(binary_search(arr, 1000, 500) == 250);
    assert(binary_search(arr, 1000, 1998) == 999);
    assert(binary_search(arr, 1000, 1) == -1);  /* Odd, not in array */

    printf(" PASSED\n");
}

int main(void) {
    printf("Running binary search tests...\n");

    test_basic_search();
    test_not_found();
    test_single_element();
    test_empty_array();
    test_recursive();
    test_left_boundary();
    test_right_boundary();
    test_all_same_elements();
    test_negative_numbers();
    test_large_array();

    printf("All binary search tests PASSED!\n");
    return 0;
}
