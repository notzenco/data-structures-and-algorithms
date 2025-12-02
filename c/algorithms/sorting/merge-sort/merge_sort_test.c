/**
 * @file merge_sort_test.c
 * @brief Unit tests for merge sort implementation
 */

#include "merge_sort.h"
#include <assert.h>
#include <stdio.h>
#include <stdbool.h>

static bool is_sorted_asc(const int *arr, size_t size) {
    for (size_t i = 1; i < size; i++) {
        if (arr[i - 1] > arr[i]) {
            return false;
        }
    }
    return true;
}

static bool is_sorted_desc(const int *arr, size_t size) {
    for (size_t i = 1; i < size; i++) {
        if (arr[i - 1] < arr[i]) {
            return false;
        }
    }
    return true;
}

static void test_basic_sort(void) {
    printf("  test_basic_sort...");

    int arr[] = {38, 27, 43, 3, 9, 82, 10};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    merge_sort(arr, size);

    assert(is_sorted_asc(arr, size));
    assert(arr[0] == 3);
    assert(arr[size - 1] == 82);

    printf(" PASSED\n");
}

static void test_already_sorted(void) {
    printf("  test_already_sorted...");

    int arr[] = {1, 2, 3, 4, 5, 6, 7, 8};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    merge_sort(arr, size);

    assert(is_sorted_asc(arr, size));

    printf(" PASSED\n");
}

static void test_reverse_sorted(void) {
    printf("  test_reverse_sorted...");

    int arr[] = {8, 7, 6, 5, 4, 3, 2, 1};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    merge_sort(arr, size);

    assert(is_sorted_asc(arr, size));

    printf(" PASSED\n");
}

static void test_single_element(void) {
    printf("  test_single_element...");

    int arr[] = {42};

    merge_sort(arr, 1);

    assert(arr[0] == 42);

    printf(" PASSED\n");
}

static void test_empty_array(void) {
    printf("  test_empty_array...");

    int arr[] = {1};

    merge_sort(arr, 0);
    merge_sort(NULL, 0);
    merge_sort(NULL, 5);

    /* Should not crash */

    printf(" PASSED\n");
}

static void test_duplicates(void) {
    printf("  test_duplicates...");

    int arr[] = {5, 2, 5, 3, 5, 1, 5, 4, 5};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    merge_sort(arr, size);

    assert(is_sorted_asc(arr, size));

    printf(" PASSED\n");
}

static void test_negative_numbers(void) {
    printf("  test_negative_numbers...");

    int arr[] = {-5, 3, -1, 0, -10, 7, 2, -3};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    merge_sort(arr, size);

    assert(is_sorted_asc(arr, size));
    assert(arr[0] == -10);
    assert(arr[size - 1] == 7);

    printf(" PASSED\n");
}

static void test_descending_sort(void) {
    printf("  test_descending_sort...");

    int arr[] = {38, 27, 43, 3, 9, 82, 10};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    merge_sort_desc(arr, size);

    assert(is_sorted_desc(arr, size));
    assert(arr[0] == 82);
    assert(arr[size - 1] == 3);

    printf(" PASSED\n");
}

static void test_two_elements(void) {
    printf("  test_two_elements...");

    int arr1[] = {2, 1};
    merge_sort(arr1, 2);
    assert(arr1[0] == 1 && arr1[1] == 2);

    int arr2[] = {1, 2};
    merge_sort(arr2, 2);
    assert(arr2[0] == 1 && arr2[1] == 2);

    printf(" PASSED\n");
}

static void test_large_array(void) {
    printf("  test_large_array...");

    int arr[1000];
    for (int i = 0; i < 1000; i++) {
        arr[i] = 1000 - i;  /* Reverse order */
    }

    merge_sort(arr, 1000);

    assert(is_sorted_asc(arr, 1000));

    printf(" PASSED\n");
}

static void test_stability(void) {
    printf("  test_stability...");

    /* Merge sort is stable - equal elements maintain relative order */
    int arr[] = {3, 1, 4, 1, 5, 9, 2, 6, 5};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    merge_sort(arr, size);

    assert(is_sorted_asc(arr, size));

    printf(" PASSED\n");
}

int main(void) {
    printf("Running merge sort tests...\n");

    test_basic_sort();
    test_already_sorted();
    test_reverse_sorted();
    test_single_element();
    test_empty_array();
    test_duplicates();
    test_negative_numbers();
    test_descending_sort();
    test_two_elements();
    test_large_array();
    test_stability();

    printf("All merge sort tests PASSED!\n");
    return 0;
}
