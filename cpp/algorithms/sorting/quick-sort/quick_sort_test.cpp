/**
 * @file quick_sort_test.cpp
 * @brief Unit tests for quick sort implementation
 */

#include "quick_sort.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <vector>

void test_empty_array() {
    std::cout << "  test_empty_array...";
    std::vector<int> arr;
    dsa::quick_sort(arr);
    assert(arr.empty());
    std::cout << " PASSED\n";
}

void test_single_element() {
    std::cout << "  test_single_element...";
    std::vector<int> arr = {42};
    dsa::quick_sort(arr);
    assert(arr[0] == 42);
    std::cout << " PASSED\n";
}

void test_two_elements() {
    std::cout << "  test_two_elements...";
    std::vector<int> arr = {2, 1};
    dsa::quick_sort(arr);
    assert(arr[0] == 1 && arr[1] == 2);
    std::cout << " PASSED\n";
}

void test_sorted_array() {
    std::cout << "  test_sorted_array...";
    std::vector<int> arr = {1, 2, 3, 4, 5};
    dsa::quick_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_reverse_sorted() {
    std::cout << "  test_reverse_sorted...";
    std::vector<int> arr = {5, 4, 3, 2, 1};
    dsa::quick_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_random_array() {
    std::cout << "  test_random_array...";
    std::vector<int> arr = {38, 27, 43, 3, 9, 82, 10};
    dsa::quick_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_duplicates() {
    std::cout << "  test_duplicates...";
    std::vector<int> arr = {5, 2, 5, 3, 5, 1, 5};
    dsa::quick_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_all_same() {
    std::cout << "  test_all_same...";
    std::vector<int> arr = {7, 7, 7, 7, 7};
    dsa::quick_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_negative_numbers() {
    std::cout << "  test_negative_numbers...";
    std::vector<int> arr = {-5, 3, -1, 0, -10, 7};
    dsa::quick_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    assert(arr[0] == -10);
    std::cout << " PASSED\n";
}

void test_descending() {
    std::cout << "  test_descending...";
    std::vector<int> arr = {1, 5, 3, 2, 4};
    dsa::quick_sort_desc(arr);
    assert(std::is_sorted(arr.begin(), arr.end(), std::greater<int>{}));
    assert(arr[0] == 5);
    std::cout << " PASSED\n";
}

void test_pointer_version() {
    std::cout << "  test_pointer_version...";
    int arr[] = {5, 2, 8, 1, 9};
    dsa::quick_sort(arr, 5);
    assert(std::is_sorted(arr, arr + 5));
    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";
    std::vector<std::string> arr = {"banana", "apple", "cherry", "date"};
    dsa::quick_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    assert(arr[0] == "apple");
    std::cout << " PASSED\n";
}

void test_large_array() {
    std::cout << "  test_large_array...";
    std::vector<int> arr;
    for (int i = 1000; i > 0; --i) {
        arr.push_back(i);
    }
    dsa::quick_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running quick sort tests...\n";
    test_empty_array();
    test_single_element();
    test_two_elements();
    test_sorted_array();
    test_reverse_sorted();
    test_random_array();
    test_duplicates();
    test_all_same();
    test_negative_numbers();
    test_descending();
    test_pointer_version();
    test_string_type();
    test_large_array();
    std::cout << "All quick sort tests PASSED!\n";
    return 0;
}
