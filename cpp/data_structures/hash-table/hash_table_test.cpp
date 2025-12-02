/**
 * @file hash_table_test.cpp
 * @brief Unit tests for HashTable implementation
 */

#include "hash_table.hpp"
#include <cassert>
#include <iostream>
#include <string>

void test_empty_table() {
    std::cout << "  test_empty_table...";
    dsa::HashTable<std::string, int> table;
    assert(table.empty());
    assert(table.size() == 0);
    assert(!table.get("key").has_value());
    assert(!table.contains("key"));
    std::cout << " PASSED\n";
}

void test_insert_get() {
    std::cout << "  test_insert_get...";
    dsa::HashTable<std::string, int> table;
    table.insert("one", 1);
    table.insert("two", 2);
    table.insert("three", 3);

    assert(table.size() == 3);
    assert(table.get("one").value() == 1);
    assert(table.get("two").value() == 2);
    assert(table.get("three").value() == 3);
    assert(!table.get("four").has_value());
    std::cout << " PASSED\n";
}

void test_update() {
    std::cout << "  test_update...";
    dsa::HashTable<std::string, int> table;
    table.insert("key", 1);
    assert(table.get("key").value() == 1);

    table.insert("key", 2);
    assert(table.get("key").value() == 2);
    assert(table.size() == 1);
    std::cout << " PASSED\n";
}

void test_remove() {
    std::cout << "  test_remove...";
    dsa::HashTable<std::string, int> table;
    table.insert("one", 1);
    table.insert("two", 2);
    table.insert("three", 3);

    assert(table.remove("two"));
    assert(table.size() == 2);
    assert(!table.get("two").has_value());
    assert(!table.remove("two"));

    assert(table.get("one").value() == 1);
    assert(table.get("three").value() == 3);
    std::cout << " PASSED\n";
}

void test_contains() {
    std::cout << "  test_contains...";
    dsa::HashTable<std::string, int> table;
    table.insert("key", 42);

    assert(table.contains("key"));
    assert(!table.contains("other"));
    std::cout << " PASSED\n";
}

void test_operator_brackets() {
    std::cout << "  test_operator_brackets...";
    dsa::HashTable<std::string, int> table;

    table["one"] = 1;
    table["two"] = 2;

    assert(table["one"] == 1);
    assert(table["two"] == 2);

    table["one"] = 10;
    assert(table["one"] == 10);

    int& val = table["three"];
    val = 3;
    assert(table["three"] == 3);
    std::cout << " PASSED\n";
}

void test_resize() {
    std::cout << "  test_resize...";
    dsa::HashTable<int, int> table(4);

    for (int i = 0; i < 100; ++i) {
        table.insert(i, i * 10);
    }

    assert(table.size() == 100);
    for (int i = 0; i < 100; ++i) {
        assert(table.get(i).value() == i * 10);
    }
    std::cout << " PASSED\n";
}

void test_clear() {
    std::cout << "  test_clear...";
    dsa::HashTable<std::string, int> table;
    table.insert("one", 1);
    table.insert("two", 2);

    table.clear();
    assert(table.empty());
    assert(table.size() == 0);
    assert(!table.contains("one"));
    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";
    dsa::HashTable<std::string, int> table1;
    table1.insert("key", 42);

    dsa::HashTable<std::string, int> table2(table1);
    assert(table2.get("key").value() == 42);

    table2.insert("key", 100);
    assert(table1.get("key").value() == 42);
    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";
    dsa::HashTable<std::string, int> table1;
    table1.insert("key", 42);

    dsa::HashTable<std::string, int> table2(std::move(table1));
    assert(table2.get("key").value() == 42);
    std::cout << " PASSED\n";
}

void test_collision_handling() {
    std::cout << "  test_collision_handling...";
    dsa::HashTable<int, std::string> table(8);

    table.insert(0, "zero");
    table.insert(8, "eight");
    table.insert(16, "sixteen");

    assert(table.get(0).value() == "zero");
    assert(table.get(8).value() == "eight");
    assert(table.get(16).value() == "sixteen");

    table.remove(8);
    assert(!table.get(8).has_value());
    assert(table.get(0).value() == "zero");
    assert(table.get(16).value() == "sixteen");
    std::cout << " PASSED\n";
}

void test_int_keys() {
    std::cout << "  test_int_keys...";
    dsa::HashTable<int, std::string> table;
    table.insert(1, "one");
    table.insert(2, "two");
    table.insert(3, "three");

    assert(table.get(1).value() == "one");
    assert(table.get(2).value() == "two");
    assert(table.get(3).value() == "three");
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running HashTable tests...\n";
    test_empty_table();
    test_insert_get();
    test_update();
    test_remove();
    test_contains();
    test_operator_brackets();
    test_resize();
    test_clear();
    test_copy_constructor();
    test_move_constructor();
    test_collision_handling();
    test_int_keys();
    std::cout << "All HashTable tests PASSED!\n";
    return 0;
}
