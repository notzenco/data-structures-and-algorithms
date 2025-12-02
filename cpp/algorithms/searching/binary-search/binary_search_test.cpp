/**
 * @file binary_search_test.cpp
 * @brief Unit tests for binary search implementation
 */

#include "binary_search.hpp"
#include <cassert>
#include <iostream>
#include <string>
#include <vector>

void test_empty_array() {
    std::cout << "  test_empty_array...";
    std::vector<int> arr;
    assert(!dsa::binary_search(arr, 5).has_value());
    std::cout << " PASSED\n";
}

void test_single_element() {
    std::cout << "  test_single_element...";
    std::vector<int> arr = {5};
    assert(dsa::binary_search(arr, 5).value() == 0);
    assert(!dsa::binary_search(arr, 3).has_value());
    std::cout << " PASSED\n";
}

void test_found() {
    std::cout << "  test_found...";
    std::vector<int> arr = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    assert(dsa::binary_search(arr, 1).value() == 0);
    assert(dsa::binary_search(arr, 5).value() == 4);
    assert(dsa::binary_search(arr, 10).value() == 9);
    std::cout << " PASSED\n";
}

void test_not_found() {
    std::cout << "  test_not_found...";
    std::vector<int> arr = {1, 3, 5, 7, 9};

    assert(!dsa::binary_search(arr, 0).has_value());
    assert(!dsa::binary_search(arr, 2).has_value());
    assert(!dsa::binary_search(arr, 10).has_value());
    std::cout << " PASSED\n";
}

void test_pointer_version() {
    std::cout << "  test_pointer_version...";
    int arr[] = {1, 2, 3, 4, 5};

    assert(dsa::binary_search(arr, 5, 3).value() == 2);
    assert(!dsa::binary_search(arr, 5, 10).has_value());
    assert(!dsa::binary_search(static_cast<int*>(nullptr), 0, 5).has_value());
    std::cout << " PASSED\n";
}

void test_lower_bound() {
    std::cout << "  test_lower_bound...";
    std::vector<int> arr = {1, 2, 4, 4, 4, 6, 7};

    assert(dsa::lower_bound(arr, 4) == 2);
    assert(dsa::lower_bound(arr, 3) == 2);
    assert(dsa::lower_bound(arr, 0) == 0);
    assert(dsa::lower_bound(arr, 10) == 7);
    std::cout << " PASSED\n";
}

void test_upper_bound() {
    std::cout << "  test_upper_bound...";
    std::vector<int> arr = {1, 2, 4, 4, 4, 6, 7};

    assert(dsa::upper_bound(arr, 4) == 5);
    assert(dsa::upper_bound(arr, 3) == 2);
    assert(dsa::upper_bound(arr, 0) == 0);
    assert(dsa::upper_bound(arr, 7) == 7);
    std::cout << " PASSED\n";
}

void test_recursive() {
    std::cout << "  test_recursive...";
    std::vector<int> arr = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    assert(dsa::binary_search_recursive(arr, 1).value() == 0);
    assert(dsa::binary_search_recursive(arr, 5).value() == 4);
    assert(dsa::binary_search_recursive(arr, 10).value() == 9);
    assert(!dsa::binary_search_recursive(arr, 0).has_value());
    std::cout << " PASSED\n";
}

void test_large_array() {
    std::cout << "  test_large_array...";
    std::vector<int> arr;
    for (int i = 0; i < 10000; ++i) {
        arr.push_back(i * 2);
    }

    assert(dsa::binary_search(arr, 0).value() == 0);
    assert(dsa::binary_search(arr, 5000).value() == 2500);
    assert(dsa::binary_search(arr, 19998).value() == 9999);
    assert(!dsa::binary_search(arr, 1).has_value());
    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";
    std::vector<std::string> arr = {"apple", "banana", "cherry", "date"};

    assert(dsa::binary_search(arr, std::string("banana")).value() == 1);
    assert(dsa::binary_search(arr, std::string("cherry")).value() == 2);
    assert(!dsa::binary_search(arr, std::string("fig")).has_value());
    std::cout << " PASSED\n";
}

void test_duplicates() {
    std::cout << "  test_duplicates...";
    std::vector<int> arr = {1, 2, 2, 2, 3, 4, 5};

    auto result = dsa::binary_search(arr, 2);
    assert(result.has_value());
    assert(result.value() >= 1 && result.value() <= 3);
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running binary search tests...\n";
    test_empty_array();
    test_single_element();
    test_found();
    test_not_found();
    test_pointer_version();
    test_lower_bound();
    test_upper_bound();
    test_recursive();
    test_large_array();
    test_string_type();
    test_duplicates();
    std::cout << "All binary search tests PASSED!\n";
    return 0;
}
