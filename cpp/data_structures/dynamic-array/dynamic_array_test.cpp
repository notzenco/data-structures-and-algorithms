/**
 * @file dynamic_array_test.cpp
 * @brief Unit tests for DynamicArray implementation
 */

#include "dynamic_array.hpp"
#include <cassert>
#include <iostream>
#include <string>

void test_empty_array() {
    std::cout << "  test_empty_array...";

    dsa::DynamicArray<int> arr;
    assert(arr.empty());
    assert(arr.size() == 0);
    assert(!arr.pop_back().has_value());
    assert(!arr.get(0).has_value());

    std::cout << " PASSED\n";
}

void test_push_pop_back() {
    std::cout << "  test_push_pop_back...";

    dsa::DynamicArray<int> arr;
    arr.push_back(1);
    arr.push_back(2);
    arr.push_back(3);

    assert(arr.size() == 3);
    assert(!arr.empty());

    assert(arr.pop_back().value() == 3);
    assert(arr.pop_back().value() == 2);
    assert(arr.pop_back().value() == 1);
    assert(arr.empty());

    std::cout << " PASSED\n";
}

void test_get_set() {
    std::cout << "  test_get_set...";

    dsa::DynamicArray<int> arr;
    arr.push_back(10);
    arr.push_back(20);
    arr.push_back(30);

    assert(arr.get(0).value() == 10);
    assert(arr.get(1).value() == 20);
    assert(arr.get(2).value() == 30);
    assert(!arr.get(3).has_value());

    assert(arr.set(1, 25));
    assert(arr.get(1).value() == 25);
    assert(!arr.set(10, 100));

    std::cout << " PASSED\n";
}

void test_operator_brackets() {
    std::cout << "  test_operator_brackets...";

    dsa::DynamicArray<int> arr;
    arr.push_back(1);
    arr.push_back(2);
    arr.push_back(3);

    assert(arr[0] == 1);
    assert(arr[1] == 2);
    assert(arr[2] == 3);

    arr[1] = 42;
    assert(arr[1] == 42);

    std::cout << " PASSED\n";
}

void test_insert() {
    std::cout << "  test_insert...";

    dsa::DynamicArray<int> arr;
    arr.push_back(1);
    arr.push_back(3);

    assert(arr.insert(1, 2));
    assert(arr.size() == 3);
    assert(arr[0] == 1);
    assert(arr[1] == 2);
    assert(arr[2] == 3);

    assert(arr.insert(0, 0));
    assert(arr[0] == 0);

    assert(!arr.insert(100, 99));

    std::cout << " PASSED\n";
}

void test_remove() {
    std::cout << "  test_remove...";

    dsa::DynamicArray<int> arr;
    arr.push_back(1);
    arr.push_back(2);
    arr.push_back(3);

    auto removed = arr.remove(1);
    assert(removed.value() == 2);
    assert(arr.size() == 2);
    assert(arr[0] == 1);
    assert(arr[1] == 3);

    assert(!arr.remove(100).has_value());

    std::cout << " PASSED\n";
}

void test_find() {
    std::cout << "  test_find...";

    dsa::DynamicArray<int> arr;
    arr.push_back(10);
    arr.push_back(20);
    arr.push_back(30);
    arr.push_back(20);

    assert(arr.find(20).value() == 1);
    assert(arr.find(30).value() == 2);
    assert(!arr.find(100).has_value());

    std::cout << " PASSED\n";
}

void test_clear() {
    std::cout << "  test_clear...";

    dsa::DynamicArray<int> arr;
    arr.push_back(1);
    arr.push_back(2);
    arr.push_back(3);

    arr.clear();
    assert(arr.empty());
    assert(arr.size() == 0);

    std::cout << " PASSED\n";
}

void test_resize() {
    std::cout << "  test_resize...";

    dsa::DynamicArray<int> arr(2);
    for (int i = 0; i < 100; ++i) {
        arr.push_back(i);
    }

    assert(arr.size() == 100);
    assert(arr.capacity() >= 100);

    for (int i = 0; i < 100; ++i) {
        assert(arr[i] == i);
    }

    std::cout << " PASSED\n";
}

void test_reserve() {
    std::cout << "  test_reserve...";

    dsa::DynamicArray<int> arr;
    arr.reserve(100);

    assert(arr.capacity() >= 100);
    assert(arr.size() == 0);

    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";

    dsa::DynamicArray<int> arr1;
    arr1.push_back(1);
    arr1.push_back(2);
    arr1.push_back(3);

    dsa::DynamicArray<int> arr2(arr1);

    assert(arr2.size() == 3);
    assert(arr2[0] == 1);
    arr2[0] = 99;
    assert(arr1[0] == 1);

    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";

    dsa::DynamicArray<int> arr1;
    arr1.push_back(1);
    arr1.push_back(2);

    dsa::DynamicArray<int> arr2(std::move(arr1));

    assert(arr2.size() == 2);
    assert(arr2[0] == 1);

    std::cout << " PASSED\n";
}

void test_iterator() {
    std::cout << "  test_iterator...";

    dsa::DynamicArray<int> arr;
    arr.push_back(1);
    arr.push_back(2);
    arr.push_back(3);

    int sum = 0;
    for (int val : arr) {
        sum += val;
    }
    assert(sum == 6);

    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";

    dsa::DynamicArray<std::string> arr;
    arr.push_back("hello");
    arr.push_back("world");

    assert(arr[0] == "hello");
    assert(arr[1] == "world");
    assert(arr.find("world").value() == 1);

    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running DynamicArray tests...\n";

    test_empty_array();
    test_push_pop_back();
    test_get_set();
    test_operator_brackets();
    test_insert();
    test_remove();
    test_find();
    test_clear();
    test_resize();
    test_reserve();
    test_copy_constructor();
    test_move_constructor();
    test_iterator();
    test_string_type();

    std::cout << "All DynamicArray tests PASSED!\n";
    return 0;
}
