/**
 * @file bst_test.cpp
 * @brief Unit tests for BST implementation
 */

#include "bst.hpp"
#include <cassert>
#include <iostream>
#include <vector>

void test_empty_tree() {
    std::cout << "  test_empty_tree...";
    dsa::BST<int> bst;
    assert(bst.empty());
    assert(bst.size() == 0);
    assert(!bst.contains(1));
    assert(!bst.min().has_value());
    assert(!bst.max().has_value());
    std::cout << " PASSED\n";
}

void test_insert() {
    std::cout << "  test_insert...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);
    bst.insert(1);
    bst.insert(9);

    assert(bst.size() == 5);
    assert(bst.contains(5));
    assert(bst.contains(3));
    assert(bst.contains(7));
    assert(!bst.contains(100));
    std::cout << " PASSED\n";
}

void test_remove() {
    std::cout << "  test_remove...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);
    bst.insert(1);
    bst.insert(4);
    bst.insert(6);
    bst.insert(9);

    assert(bst.remove(3));
    assert(!bst.contains(3));
    assert(bst.size() == 6);

    assert(bst.remove(5));
    assert(!bst.contains(5));

    assert(!bst.remove(100));
    std::cout << " PASSED\n";
}

void test_min_max() {
    std::cout << "  test_min_max...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);
    bst.insert(1);
    bst.insert(9);

    assert(bst.min().value() == 1);
    assert(bst.max().value() == 9);
    std::cout << " PASSED\n";
}

void test_inorder() {
    std::cout << "  test_inorder...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);
    bst.insert(1);
    bst.insert(9);

    std::vector<int> expected = {1, 3, 5, 7, 9};
    assert(bst.inorder() == expected);
    std::cout << " PASSED\n";
}

void test_preorder() {
    std::cout << "  test_preorder...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);

    std::vector<int> expected = {5, 3, 7};
    assert(bst.preorder() == expected);
    std::cout << " PASSED\n";
}

void test_postorder() {
    std::cout << "  test_postorder...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);

    std::vector<int> expected = {3, 7, 5};
    assert(bst.postorder() == expected);
    std::cout << " PASSED\n";
}

void test_height() {
    std::cout << "  test_height...";
    dsa::BST<int> bst;
    assert(bst.height() == 0);

    bst.insert(5);
    assert(bst.height() == 1);

    bst.insert(3);
    bst.insert(7);
    assert(bst.height() == 2);

    bst.insert(1);
    assert(bst.height() == 3);
    std::cout << " PASSED\n";
}

void test_find() {
    std::cout << "  test_find...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);

    assert(bst.find(5).value() == 5);
    assert(bst.find(3).value() == 3);
    assert(!bst.find(100).has_value());
    std::cout << " PASSED\n";
}

void test_clear() {
    std::cout << "  test_clear...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(3);
    bst.insert(7);

    bst.clear();
    assert(bst.empty());
    assert(bst.size() == 0);
    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";
    dsa::BST<int> bst1;
    bst1.insert(5);
    bst1.insert(3);
    bst1.insert(7);

    dsa::BST<int> bst2(bst1);
    assert(bst2.size() == 3);
    assert(bst2.contains(5));

    bst2.remove(5);
    assert(bst1.contains(5));
    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";
    dsa::BST<int> bst1;
    bst1.insert(5);
    bst1.insert(3);

    dsa::BST<int> bst2(std::move(bst1));
    assert(bst2.size() == 2);
    assert(bst2.contains(5));
    std::cout << " PASSED\n";
}

void test_duplicate_insert() {
    std::cout << "  test_duplicate_insert...";
    dsa::BST<int> bst;
    bst.insert(5);
    bst.insert(5);
    bst.insert(5);

    assert(bst.size() == 3);
    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";
    dsa::BST<std::string> bst;
    bst.insert("banana");
    bst.insert("apple");
    bst.insert("cherry");

    assert(bst.min().value() == "apple");
    assert(bst.max().value() == "cherry");

    std::vector<std::string> expected = {"apple", "banana", "cherry"};
    assert(bst.inorder() == expected);
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running BST tests...\n";
    test_empty_tree();
    test_insert();
    test_remove();
    test_min_max();
    test_inorder();
    test_preorder();
    test_postorder();
    test_height();
    test_find();
    test_clear();
    test_copy_constructor();
    test_move_constructor();
    test_duplicate_insert();
    test_string_type();
    std::cout << "All BST tests PASSED!\n";
    return 0;
}
