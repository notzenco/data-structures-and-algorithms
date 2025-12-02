/**
 * @file queue_test.cpp
 * @brief Unit tests for Queue implementation
 */

#include "queue.hpp"
#include <cassert>
#include <iostream>
#include <string>

void test_empty_queue() {
    std::cout << "  test_empty_queue...";

    dsa::Queue<int> queue;
    assert(queue.empty());
    assert(queue.size() == 0);
    assert(!queue.dequeue().has_value());
    assert(!queue.front().has_value());

    std::cout << " PASSED\n";
}

void test_enqueue_dequeue() {
    std::cout << "  test_enqueue_dequeue...";

    dsa::Queue<int> queue;
    queue.enqueue(1);
    queue.enqueue(2);
    queue.enqueue(3);

    assert(queue.size() == 3);
    assert(!queue.empty());

    auto val = queue.dequeue();
    assert(val.has_value() && val.value() == 1);

    val = queue.dequeue();
    assert(val.has_value() && val.value() == 2);

    val = queue.dequeue();
    assert(val.has_value() && val.value() == 3);

    assert(queue.empty());

    std::cout << " PASSED\n";
}

void test_front_back() {
    std::cout << "  test_front_back...";

    dsa::Queue<int> queue;
    queue.enqueue(1);
    queue.enqueue(2);
    queue.enqueue(3);

    auto front = queue.front();
    assert(front.has_value() && front.value() == 1);

    auto back = queue.back();
    assert(back.has_value() && back.value() == 3);

    assert(queue.size() == 3);

    std::cout << " PASSED\n";
}

void test_clear() {
    std::cout << "  test_clear...";

    dsa::Queue<int> queue;
    queue.enqueue(1);
    queue.enqueue(2);
    queue.enqueue(3);

    queue.clear();
    assert(queue.empty());
    assert(queue.size() == 0);

    std::cout << " PASSED\n";
}

void test_resize() {
    std::cout << "  test_resize...";

    dsa::Queue<int> queue(2);
    for (int i = 0; i < 100; ++i) {
        queue.enqueue(i);
    }

    assert(queue.size() == 100);

    for (int i = 0; i < 100; ++i) {
        auto val = queue.dequeue();
        assert(val.has_value() && val.value() == i);
    }

    std::cout << " PASSED\n";
}

void test_circular_behavior() {
    std::cout << "  test_circular_behavior...";

    dsa::Queue<int> queue(4);

    queue.enqueue(1);
    queue.enqueue(2);
    queue.dequeue();
    queue.dequeue();

    queue.enqueue(3);
    queue.enqueue(4);
    queue.enqueue(5);

    assert(queue.size() == 3);
    assert(queue.dequeue().value() == 3);
    assert(queue.dequeue().value() == 4);
    assert(queue.dequeue().value() == 5);

    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";

    dsa::Queue<int> queue1;
    queue1.enqueue(1);
    queue1.enqueue(2);
    queue1.enqueue(3);

    dsa::Queue<int> queue2(queue1);

    assert(queue2.size() == 3);
    assert(queue2.dequeue().value() == 1);
    assert(queue1.size() == 3);

    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";

    dsa::Queue<int> queue1;
    queue1.enqueue(1);
    queue1.enqueue(2);

    dsa::Queue<int> queue2(std::move(queue1));

    assert(queue2.size() == 2);
    assert(queue2.dequeue().value() == 1);

    std::cout << " PASSED\n";
}

void test_copy_assignment() {
    std::cout << "  test_copy_assignment...";

    dsa::Queue<int> queue1;
    queue1.enqueue(1);
    queue1.enqueue(2);

    dsa::Queue<int> queue2;
    queue2.enqueue(99);
    queue2 = queue1;

    assert(queue2.size() == 2);
    assert(queue2.dequeue().value() == 1);
    assert(queue1.size() == 2);

    std::cout << " PASSED\n";
}

void test_move_assignment() {
    std::cout << "  test_move_assignment...";

    dsa::Queue<int> queue1;
    queue1.enqueue(1);
    queue1.enqueue(2);

    dsa::Queue<int> queue2;
    queue2.enqueue(99);
    queue2 = std::move(queue1);

    assert(queue2.size() == 2);
    assert(queue2.dequeue().value() == 1);

    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";

    dsa::Queue<std::string> queue;
    queue.enqueue("hello");
    queue.enqueue("world");

    assert(queue.dequeue().value() == "hello");
    assert(queue.dequeue().value() == "world");

    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running Queue tests...\n";

    test_empty_queue();
    test_enqueue_dequeue();
    test_front_back();
    test_clear();
    test_resize();
    test_circular_behavior();
    test_copy_constructor();
    test_move_constructor();
    test_copy_assignment();
    test_move_assignment();
    test_string_type();

    std::cout << "All Queue tests PASSED!\n";
    return 0;
}
