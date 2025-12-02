/**
 * @file disjoint_set_test.cpp
 * @brief Unit tests for DisjointSet implementation
 */

#include "disjoint_set.hpp"
#include <cassert>
#include <iostream>

void test_initial_state() {
    std::cout << "  test_initial_state...";
    dsa::DisjointSet ds(5);

    assert(ds.size() == 5);
    assert(ds.num_sets() == 5);

    for (std::size_t i = 0; i < 5; ++i) {
        assert(ds.find(i) == i);
        assert(ds.set_size(i) == 1);
    }
    std::cout << " PASSED\n";
}

void test_unite() {
    std::cout << "  test_unite...";
    dsa::DisjointSet ds(5);

    assert(ds.unite(0, 1));
    assert(ds.num_sets() == 4);
    assert(ds.connected(0, 1));
    assert(!ds.connected(0, 2));

    assert(ds.unite(2, 3));
    assert(ds.num_sets() == 3);

    assert(ds.unite(0, 2));
    assert(ds.num_sets() == 2);
    assert(ds.connected(0, 3));
    assert(ds.connected(1, 2));
    std::cout << " PASSED\n";
}

void test_unite_same_set() {
    std::cout << "  test_unite_same_set...";
    dsa::DisjointSet ds(5);

    ds.unite(0, 1);
    assert(!ds.unite(0, 1));
    assert(!ds.unite(1, 0));
    assert(ds.num_sets() == 4);
    std::cout << " PASSED\n";
}

void test_connected() {
    std::cout << "  test_connected...";
    dsa::DisjointSet ds(5);

    assert(!ds.connected(0, 1));
    ds.unite(0, 1);
    assert(ds.connected(0, 1));
    assert(ds.connected(1, 0));

    ds.unite(1, 2);
    assert(ds.connected(0, 2));
    std::cout << " PASSED\n";
}

void test_set_size() {
    std::cout << "  test_set_size...";
    dsa::DisjointSet ds(5);

    assert(ds.set_size(0) == 1);

    ds.unite(0, 1);
    assert(ds.set_size(0) == 2);
    assert(ds.set_size(1) == 2);

    ds.unite(0, 2);
    assert(ds.set_size(0) == 3);
    assert(ds.set_size(1) == 3);
    assert(ds.set_size(2) == 3);
    std::cout << " PASSED\n";
}

void test_path_compression() {
    std::cout << "  test_path_compression...";
    dsa::DisjointSet ds(10);

    for (std::size_t i = 0; i < 9; ++i) {
        ds.unite(i, i + 1);
    }

    std::size_t root = ds.find(0);
    for (std::size_t i = 0; i < 10; ++i) {
        assert(ds.find(i) == root);
    }
    std::cout << " PASSED\n";
}

void test_large_set() {
    std::cout << "  test_large_set...";
    dsa::DisjointSet ds(1000);

    for (std::size_t i = 0; i < 999; ++i) {
        ds.unite(i, i + 1);
    }

    assert(ds.num_sets() == 1);
    assert(ds.set_size(0) == 1000);

    for (std::size_t i = 0; i < 1000; ++i) {
        assert(ds.connected(0, i));
    }
    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";
    dsa::DisjointSet ds1(5);
    ds1.unite(0, 1);
    ds1.unite(2, 3);

    dsa::DisjointSet ds2(ds1);
    assert(ds2.num_sets() == 3);
    assert(ds2.connected(0, 1));

    ds2.unite(0, 2);
    assert(ds1.num_sets() == 3);
    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";
    dsa::DisjointSet ds1(5);
    ds1.unite(0, 1);

    dsa::DisjointSet ds2(std::move(ds1));
    assert(ds2.num_sets() == 4);
    assert(ds2.connected(0, 1));
    std::cout << " PASSED\n";
}

void test_multiple_components() {
    std::cout << "  test_multiple_components...";
    dsa::DisjointSet ds(10);

    ds.unite(0, 1);
    ds.unite(1, 2);

    ds.unite(3, 4);
    ds.unite(4, 5);

    ds.unite(6, 7);

    assert(ds.num_sets() == 5);
    assert(ds.set_size(0) == 3);
    assert(ds.set_size(3) == 3);
    assert(ds.set_size(6) == 2);
    assert(ds.set_size(8) == 1);
    assert(ds.set_size(9) == 1);

    assert(!ds.connected(0, 3));
    assert(!ds.connected(3, 6));
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running DisjointSet tests...\n";
    test_initial_state();
    test_unite();
    test_unite_same_set();
    test_connected();
    test_set_size();
    test_path_compression();
    test_large_set();
    test_copy_constructor();
    test_move_constructor();
    test_multiple_components();
    std::cout << "All DisjointSet tests PASSED!\n";
    return 0;
}
