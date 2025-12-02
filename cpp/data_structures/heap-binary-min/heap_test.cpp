/**
 * @file heap_test.cpp
 * @brief Unit tests for MinHeap implementation
 */

#include "heap.hpp"
#include <cassert>
#include <iostream>
#include <vector>

void test_empty_heap() {
    std::cout << "  test_empty_heap...";
    dsa::MinHeap<int> heap;
    assert(heap.empty());
    assert(heap.size() == 0);
    assert(!heap.peek().has_value());
    assert(!heap.extract_min().has_value());
    std::cout << " PASSED\n";
}

void test_insert() {
    std::cout << "  test_insert...";
    dsa::MinHeap<int> heap;
    heap.insert(5);
    heap.insert(3);
    heap.insert(7);
    heap.insert(1);

    assert(heap.size() == 4);
    assert(heap.peek().value() == 1);
    std::cout << " PASSED\n";
}

void test_extract_min() {
    std::cout << "  test_extract_min...";
    dsa::MinHeap<int> heap;
    heap.insert(5);
    heap.insert(3);
    heap.insert(7);
    heap.insert(1);
    heap.insert(9);

    assert(heap.extract_min().value() == 1);
    assert(heap.extract_min().value() == 3);
    assert(heap.extract_min().value() == 5);
    assert(heap.extract_min().value() == 7);
    assert(heap.extract_min().value() == 9);
    assert(heap.empty());
    std::cout << " PASSED\n";
}

void test_peek() {
    std::cout << "  test_peek...";
    dsa::MinHeap<int> heap;
    heap.insert(5);
    heap.insert(3);

    assert(heap.peek().value() == 3);
    assert(heap.size() == 2);
    assert(heap.peek().value() == 3);
    std::cout << " PASSED\n";
}

void test_decrease_key() {
    std::cout << "  test_decrease_key...";
    dsa::MinHeap<int> heap;
    heap.insert(5);
    heap.insert(3);
    heap.insert(7);

    heap.decrease_key(2, 1);
    assert(heap.peek().value() == 1);
    std::cout << " PASSED\n";
}

void test_heapify() {
    std::cout << "  test_heapify...";
    std::vector<int> values = {5, 3, 7, 1, 9, 2, 8};
    auto heap = dsa::MinHeap<int>::heapify(values);

    assert(heap.size() == 7);
    assert(heap.extract_min().value() == 1);
    assert(heap.extract_min().value() == 2);
    assert(heap.extract_min().value() == 3);
    std::cout << " PASSED\n";
}

void test_clear() {
    std::cout << "  test_clear...";
    dsa::MinHeap<int> heap;
    heap.insert(1);
    heap.insert(2);
    heap.insert(3);

    heap.clear();
    assert(heap.empty());
    assert(heap.size() == 0);
    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";
    dsa::MinHeap<int> heap1;
    heap1.insert(3);
    heap1.insert(1);
    heap1.insert(2);

    dsa::MinHeap<int> heap2(heap1);
    assert(heap2.size() == 3);
    assert(heap2.peek().value() == 1);

    heap2.extract_min();
    assert(heap1.peek().value() == 1);
    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";
    dsa::MinHeap<int> heap1;
    heap1.insert(3);
    heap1.insert(1);

    dsa::MinHeap<int> heap2(std::move(heap1));
    assert(heap2.size() == 2);
    assert(heap2.peek().value() == 1);
    std::cout << " PASSED\n";
}

void test_large_heap() {
    std::cout << "  test_large_heap...";
    dsa::MinHeap<int> heap;
    for (int i = 100; i >= 1; --i) {
        heap.insert(i);
    }

    assert(heap.size() == 100);
    for (int i = 1; i <= 100; ++i) {
        assert(heap.extract_min().value() == i);
    }
    std::cout << " PASSED\n";
}

void test_duplicates() {
    std::cout << "  test_duplicates...";
    dsa::MinHeap<int> heap;
    heap.insert(5);
    heap.insert(5);
    heap.insert(5);
    heap.insert(3);
    heap.insert(3);

    assert(heap.extract_min().value() == 3);
    assert(heap.extract_min().value() == 3);
    assert(heap.extract_min().value() == 5);
    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";
    dsa::MinHeap<std::string> heap;
    heap.insert("banana");
    heap.insert("apple");
    heap.insert("cherry");

    assert(heap.extract_min().value() == "apple");
    assert(heap.extract_min().value() == "banana");
    assert(heap.extract_min().value() == "cherry");
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running MinHeap tests...\n";
    test_empty_heap();
    test_insert();
    test_extract_min();
    test_peek();
    test_decrease_key();
    test_heapify();
    test_clear();
    test_copy_constructor();
    test_move_constructor();
    test_large_heap();
    test_duplicates();
    test_string_type();
    std::cout << "All MinHeap tests PASSED!\n";
    return 0;
}
