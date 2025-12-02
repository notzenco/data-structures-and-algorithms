/**
 * @file deque_test.cpp
 * @brief Unit tests for Deque implementation
 */

#include "deque.hpp"
#include <cassert>
#include <iostream>
#include <string>

void test_empty_deque() {
    std::cout << "  test_empty_deque...";
    dsa::Deque<int> deque;
    assert(deque.empty());
    assert(deque.size() == 0);
    assert(!deque.pop_front().has_value());
    assert(!deque.pop_back().has_value());
    std::cout << " PASSED\n";
}

void test_push_front() {
    std::cout << "  test_push_front...";
    dsa::Deque<int> deque;
    deque.push_front(3);
    deque.push_front(2);
    deque.push_front(1);
    assert(deque.size() == 3);
    assert(deque.front().value() == 1);
    assert(deque.back().value() == 3);
    std::cout << " PASSED\n";
}

void test_push_back() {
    std::cout << "  test_push_back...";
    dsa::Deque<int> deque;
    deque.push_back(1);
    deque.push_back(2);
    deque.push_back(3);
    assert(deque.size() == 3);
    assert(deque.front().value() == 1);
    assert(deque.back().value() == 3);
    std::cout << " PASSED\n";
}

void test_pop_front() {
    std::cout << "  test_pop_front...";
    dsa::Deque<int> deque;
    deque.push_back(1);
    deque.push_back(2);
    deque.push_back(3);
    assert(deque.pop_front().value() == 1);
    assert(deque.pop_front().value() == 2);
    assert(deque.pop_front().value() == 3);
    assert(deque.empty());
    std::cout << " PASSED\n";
}

void test_pop_back() {
    std::cout << "  test_pop_back...";
    dsa::Deque<int> deque;
    deque.push_back(1);
    deque.push_back(2);
    deque.push_back(3);
    assert(deque.pop_back().value() == 3);
    assert(deque.pop_back().value() == 2);
    assert(deque.pop_back().value() == 1);
    assert(deque.empty());
    std::cout << " PASSED\n";
}

void test_mixed_operations() {
    std::cout << "  test_mixed_operations...";
    dsa::Deque<int> deque;
    deque.push_back(2);
    deque.push_front(1);
    deque.push_back(3);
    assert(deque.front().value() == 1);
    assert(deque.back().value() == 3);
    assert(deque.pop_front().value() == 1);
    assert(deque.pop_back().value() == 3);
    assert(deque.size() == 1);
    std::cout << " PASSED\n";
}

void test_get() {
    std::cout << "  test_get...";
    dsa::Deque<int> deque;
    deque.push_back(10);
    deque.push_back(20);
    deque.push_back(30);
    assert(deque.get(0).value() == 10);
    assert(deque.get(1).value() == 20);
    assert(deque.get(2).value() == 30);
    assert(!deque.get(3).has_value());
    std::cout << " PASSED\n";
}

void test_resize() {
    std::cout << "  test_resize...";
    dsa::Deque<int> deque(2);
    for (int i = 0; i < 100; ++i) {
        deque.push_back(i);
    }
    assert(deque.size() == 100);
    for (int i = 0; i < 100; ++i) {
        assert(deque.pop_front().value() == i);
    }
    std::cout << " PASSED\n";
}

void test_clear() {
    std::cout << "  test_clear...";
    dsa::Deque<int> deque;
    deque.push_back(1);
    deque.push_back(2);
    deque.clear();
    assert(deque.empty());
    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";
    dsa::Deque<int> deque1;
    deque1.push_back(1);
    deque1.push_back(2);
    dsa::Deque<int> deque2(deque1);
    assert(deque2.size() == 2);
    deque2.pop_front();
    assert(deque1.size() == 2);
    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";
    dsa::Deque<int> deque1;
    deque1.push_back(1);
    deque1.push_back(2);
    dsa::Deque<int> deque2(std::move(deque1));
    assert(deque2.size() == 2);
    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";
    dsa::Deque<std::string> deque;
    deque.push_back("hello");
    deque.push_back("world");
    assert(deque.front().value() == "hello");
    assert(deque.back().value() == "world");
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running Deque tests...\n";
    test_empty_deque();
    test_push_front();
    test_push_back();
    test_pop_front();
    test_pop_back();
    test_mixed_operations();
    test_get();
    test_resize();
    test_clear();
    test_copy_constructor();
    test_move_constructor();
    test_string_type();
    std::cout << "All Deque tests PASSED!\n";
    return 0;
}
