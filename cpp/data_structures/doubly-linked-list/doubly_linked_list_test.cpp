/**
 * @file doubly_linked_list_test.cpp
 * @brief Unit tests for DoublyLinkedList implementation
 */

#include "doubly_linked_list.hpp"
#include <cassert>
#include <iostream>
#include <string>

void test_empty_list() {
    std::cout << "  test_empty_list...";
    dsa::DoublyLinkedList<int> list;
    assert(list.empty());
    assert(list.size() == 0);
    assert(!list.pop_front().has_value());
    assert(!list.pop_back().has_value());
    std::cout << " PASSED\n";
}

void test_push_front() {
    std::cout << "  test_push_front...";
    dsa::DoublyLinkedList<int> list;
    list.push_front(3);
    list.push_front(2);
    list.push_front(1);
    assert(list.size() == 3);
    assert(list.front().value() == 1);
    assert(list.back().value() == 3);
    std::cout << " PASSED\n";
}

void test_push_back() {
    std::cout << "  test_push_back...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(1);
    list.push_back(2);
    list.push_back(3);
    assert(list.size() == 3);
    assert(list.front().value() == 1);
    assert(list.back().value() == 3);
    std::cout << " PASSED\n";
}

void test_pop_front() {
    std::cout << "  test_pop_front...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(1);
    list.push_back(2);
    list.push_back(3);
    assert(list.pop_front().value() == 1);
    assert(list.pop_front().value() == 2);
    assert(list.pop_front().value() == 3);
    assert(list.empty());
    std::cout << " PASSED\n";
}

void test_pop_back() {
    std::cout << "  test_pop_back...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(1);
    list.push_back(2);
    list.push_back(3);
    assert(list.pop_back().value() == 3);
    assert(list.pop_back().value() == 2);
    assert(list.pop_back().value() == 1);
    assert(list.empty());
    std::cout << " PASSED\n";
}

void test_get() {
    std::cout << "  test_get...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(10);
    list.push_back(20);
    list.push_back(30);
    assert(list.get(0).value() == 10);
    assert(list.get(1).value() == 20);
    assert(list.get(2).value() == 30);
    assert(!list.get(3).has_value());
    std::cout << " PASSED\n";
}

void test_insert() {
    std::cout << "  test_insert...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(1);
    list.push_back(3);
    assert(list.insert(1, 2));
    assert(list.size() == 3);
    assert(list.get(1).value() == 2);
    assert(list.insert(0, 0));
    assert(list.front().value() == 0);
    assert(list.insert(4, 4));
    assert(list.back().value() == 4);
    std::cout << " PASSED\n";
}

void test_remove() {
    std::cout << "  test_remove...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(1);
    list.push_back(2);
    list.push_back(3);
    list.push_back(4);
    assert(list.remove(1).value() == 2);
    assert(list.size() == 3);
    assert(list.get(1).value() == 3);
    std::cout << " PASSED\n";
}

void test_find() {
    std::cout << "  test_find...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(10);
    list.push_back(20);
    list.push_back(30);
    assert(list.find(20).value() == 1);
    assert(!list.find(100).has_value());
    std::cout << " PASSED\n";
}

void test_reverse() {
    std::cout << "  test_reverse...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(1);
    list.push_back(2);
    list.push_back(3);
    list.reverse();
    assert(list.get(0).value() == 3);
    assert(list.get(1).value() == 2);
    assert(list.get(2).value() == 1);
    assert(list.front().value() == 3);
    assert(list.back().value() == 1);
    std::cout << " PASSED\n";
}

void test_clear() {
    std::cout << "  test_clear...";
    dsa::DoublyLinkedList<int> list;
    list.push_back(1);
    list.push_back(2);
    list.clear();
    assert(list.empty());
    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";
    dsa::DoublyLinkedList<int> list1;
    list1.push_back(1);
    list1.push_back(2);
    dsa::DoublyLinkedList<int> list2(list1);
    assert(list2.size() == 2);
    list2.pop_front();
    assert(list1.size() == 2);
    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";
    dsa::DoublyLinkedList<int> list1;
    list1.push_back(1);
    list1.push_back(2);
    dsa::DoublyLinkedList<int> list2(std::move(list1));
    assert(list2.size() == 2);
    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";
    dsa::DoublyLinkedList<std::string> list;
    list.push_back("hello");
    list.push_back("world");
    assert(list.front().value() == "hello");
    assert(list.back().value() == "world");
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running DoublyLinkedList tests...\n";
    test_empty_list();
    test_push_front();
    test_push_back();
    test_pop_front();
    test_pop_back();
    test_get();
    test_insert();
    test_remove();
    test_find();
    test_reverse();
    test_clear();
    test_copy_constructor();
    test_move_constructor();
    test_string_type();
    std::cout << "All DoublyLinkedList tests PASSED!\n";
    return 0;
}
