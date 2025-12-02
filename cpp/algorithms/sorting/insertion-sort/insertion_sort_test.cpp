/**
 * @file insertion_sort_test.cpp
 * @brief Unit tests for insertion sort implementation
 */

#include "insertion_sort.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <vector>

void test_empty_array() {
    std::cout << "  test_empty_array...";
    std::vector<int> arr;
    dsa::insertion_sort(arr);
    assert(arr.empty());
    std::cout << " PASSED\n";
}

void test_single_element() {
    std::cout << "  test_single_element...";
    std::vector<int> arr = {42};
    dsa::insertion_sort(arr);
    assert(arr[0] == 42);
    std::cout << " PASSED\n";
}

void test_sorted_array() {
    std::cout << "  test_sorted_array...";
    std::vector<int> arr = {1, 2, 3, 4, 5};
    dsa::insertion_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_reverse_sorted() {
    std::cout << "  test_reverse_sorted...";
    std::vector<int> arr = {5, 4, 3, 2, 1};
    dsa::insertion_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_random_array() {
    std::cout << "  test_random_array...";
    std::vector<int> arr = {38, 27, 43, 3, 9, 82, 10};
    dsa::insertion_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_duplicates() {
    std::cout << "  test_duplicates...";
    std::vector<int> arr = {5, 2, 5, 3, 5, 1, 5};
    dsa::insertion_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    std::cout << " PASSED\n";
}

void test_negative_numbers() {
    std::cout << "  test_negative_numbers...";
    std::vector<int> arr = {-5, 3, -1, 0, -10, 7};
    dsa::insertion_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    assert(arr[0] == -10);
    std::cout << " PASSED\n";
}

void test_descending() {
    std::cout << "  test_descending...";
    std::vector<int> arr = {1, 5, 3, 2, 4};
    dsa::insertion_sort_desc(arr);
    assert(std::is_sorted(arr.begin(), arr.end(), std::greater<int>{}));
    assert(arr[0] == 5);
    std::cout << " PASSED\n";
}

void test_pointer_version() {
    std::cout << "  test_pointer_version...";
    int arr[] = {5, 2, 8, 1, 9};
    dsa::insertion_sort(arr, 5);
    assert(std::is_sorted(arr, arr + 5));
    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";
    std::vector<std::string> arr = {"banana", "apple", "cherry", "date"};
    dsa::insertion_sort(arr);
    assert(std::is_sorted(arr.begin(), arr.end()));
    assert(arr[0] == "apple");
    std::cout << " PASSED\n";
}

void test_stability() {
    std::cout << "  test_stability...";
    std::vector<std::pair<int, char>> arr = {{1, 'a'}, {2, 'b'}, {1, 'c'}, {2, 'd'}};
    dsa::insertion_sort(arr, [](const auto& a, const auto& b) {
        return a.first < b.first;
    });
    assert(arr[0].second == 'a');
    assert(arr[1].second == 'c');
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running insertion sort tests...\n";
    test_empty_array();
    test_single_element();
    test_sorted_array();
    test_reverse_sorted();
    test_random_array();
    test_duplicates();
    test_negative_numbers();
    test_descending();
    test_pointer_version();
    test_string_type();
    test_stability();
    std::cout << "All insertion sort tests PASSED!\n";
    return 0;
}
