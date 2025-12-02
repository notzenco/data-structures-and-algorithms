/**
 * @file insertion_sort_test.c
 * @brief Unit tests for insertion sort implementation
 */

#include "insertion_sort.h"
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

    int arr[] = {5, 2, 8, 1, 9, 3};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    insertion_sort(arr, size);

    assert(is_sorted_asc(arr, size));
    assert(arr[0] == 1);
    assert(arr[5] == 9);

    printf(" PASSED\n");
}

static void test_already_sorted(void) {
    printf("  test_already_sorted...");

    int arr[] = {1, 2, 3, 4, 5};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    insertion_sort(arr, size);

    assert(is_sorted_asc(arr, size));

    printf(" PASSED\n");
}

static void test_reverse_sorted(void) {
    printf("  test_reverse_sorted...");

    int arr[] = {5, 4, 3, 2, 1};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    insertion_sort(arr, size);

    assert(is_sorted_asc(arr, size));

    printf(" PASSED\n");
}

static void test_single_element(void) {
    printf("  test_single_element...");

    int arr[] = {42};

    insertion_sort(arr, 1);

    assert(arr[0] == 42);

    printf(" PASSED\n");
}

static void test_empty_array(void) {
    printf("  test_empty_array...");

    int arr[] = {1};

    insertion_sort(arr, 0);
    insertion_sort(NULL, 0);
    insertion_sort(NULL, 5);

    /* Should not crash */

    printf(" PASSED\n");
}

static void test_duplicates(void) {
    printf("  test_duplicates...");

    int arr[] = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    insertion_sort(arr, size);

    assert(is_sorted_asc(arr, size));

    printf(" PASSED\n");
}

static void test_negative_numbers(void) {
    printf("  test_negative_numbers...");

    int arr[] = {-5, 3, -1, 0, -10, 7, 2};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    insertion_sort(arr, size);

    assert(is_sorted_asc(arr, size));
    assert(arr[0] == -10);
    assert(arr[size - 1] == 7);

    printf(" PASSED\n");
}

static void test_descending_sort(void) {
    printf("  test_descending_sort...");

    int arr[] = {5, 2, 8, 1, 9, 3};
    size_t size = sizeof(arr) / sizeof(arr[0]);

    insertion_sort_desc(arr, size);

    assert(is_sorted_desc(arr, size));
    assert(arr[0] == 9);
    assert(arr[5] == 1);

    printf(" PASSED\n");
}

static void test_two_elements(void) {
    printf("  test_two_elements...");

    int arr1[] = {2, 1};
    insertion_sort(arr1, 2);
    assert(arr1[0] == 1 && arr1[1] == 2);

    int arr2[] = {1, 2};
    insertion_sort(arr2, 2);
    assert(arr2[0] == 1 && arr2[1] == 2);

    printf(" PASSED\n");
}

static void test_large_array(void) {
    printf("  test_large_array...");

    int arr[100];
    for (int i = 0; i < 100; i++) {
        arr[i] = 100 - i;  /* Reverse order */
    }

    insertion_sort(arr, 100);

    assert(is_sorted_asc(arr, 100));

    printf(" PASSED\n");
}

int main(void) {
    printf("Running insertion sort tests...\n");

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

    printf("All insertion sort tests PASSED!\n");
    return 0;
}
