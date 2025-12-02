/**
 * @file stack_test.cpp
 * @brief Unit tests for Stack implementation
 */

#include "stack.hpp"
#include <cassert>
#include <iostream>
#include <string>

void test_empty_stack() {
    std::cout << "  test_empty_stack...";

    dsa::Stack<int> stack;
    assert(stack.empty());
    assert(stack.size() == 0);
    assert(!stack.pop().has_value());
    assert(!stack.peek().has_value());

    std::cout << " PASSED\n";
}

void test_push_pop() {
    std::cout << "  test_push_pop...";

    dsa::Stack<int> stack;
    stack.push(1);
    stack.push(2);
    stack.push(3);

    assert(stack.size() == 3);
    assert(!stack.empty());

    auto val = stack.pop();
    assert(val.has_value() && val.value() == 3);

    val = stack.pop();
    assert(val.has_value() && val.value() == 2);

    val = stack.pop();
    assert(val.has_value() && val.value() == 1);

    assert(stack.empty());

    std::cout << " PASSED\n";
}

void test_peek() {
    std::cout << "  test_peek...";

    dsa::Stack<int> stack;
    stack.push(42);

    auto val = stack.peek();
    assert(val.has_value() && val.value() == 42);
    assert(stack.size() == 1);

    val = stack.peek();
    assert(val.has_value() && val.value() == 42);
    assert(stack.size() == 1);

    std::cout << " PASSED\n";
}

void test_clear() {
    std::cout << "  test_clear...";

    dsa::Stack<int> stack;
    stack.push(1);
    stack.push(2);
    stack.push(3);

    stack.clear();
    assert(stack.empty());
    assert(stack.size() == 0);

    std::cout << " PASSED\n";
}

void test_resize() {
    std::cout << "  test_resize...";

    dsa::Stack<int> stack(2);
    for (int i = 0; i < 100; ++i) {
        stack.push(i);
    }

    assert(stack.size() == 100);

    for (int i = 99; i >= 0; --i) {
        auto val = stack.pop();
        assert(val.has_value() && val.value() == i);
    }

    std::cout << " PASSED\n";
}

void test_copy_constructor() {
    std::cout << "  test_copy_constructor...";

    dsa::Stack<int> stack1;
    stack1.push(1);
    stack1.push(2);
    stack1.push(3);

    dsa::Stack<int> stack2(stack1);

    assert(stack2.size() == 3);
    assert(stack2.pop().value() == 3);
    assert(stack1.size() == 3);

    std::cout << " PASSED\n";
}

void test_move_constructor() {
    std::cout << "  test_move_constructor...";

    dsa::Stack<int> stack1;
    stack1.push(1);
    stack1.push(2);

    dsa::Stack<int> stack2(std::move(stack1));

    assert(stack2.size() == 2);
    assert(stack2.pop().value() == 2);

    std::cout << " PASSED\n";
}

void test_copy_assignment() {
    std::cout << "  test_copy_assignment...";

    dsa::Stack<int> stack1;
    stack1.push(1);
    stack1.push(2);

    dsa::Stack<int> stack2;
    stack2.push(99);
    stack2 = stack1;

    assert(stack2.size() == 2);
    assert(stack2.pop().value() == 2);
    assert(stack1.size() == 2);

    std::cout << " PASSED\n";
}

void test_move_assignment() {
    std::cout << "  test_move_assignment...";

    dsa::Stack<int> stack1;
    stack1.push(1);
    stack1.push(2);

    dsa::Stack<int> stack2;
    stack2.push(99);
    stack2 = std::move(stack1);

    assert(stack2.size() == 2);
    assert(stack2.pop().value() == 2);

    std::cout << " PASSED\n";
}

void test_string_type() {
    std::cout << "  test_string_type...";

    dsa::Stack<std::string> stack;
    stack.push("hello");
    stack.push("world");

    assert(stack.pop().value() == "world");
    assert(stack.pop().value() == "hello");

    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running Stack tests...\n";

    test_empty_stack();
    test_push_pop();
    test_peek();
    test_clear();
    test_resize();
    test_copy_constructor();
    test_move_constructor();
    test_copy_assignment();
    test_move_assignment();
    test_string_type();

    std::cout << "All Stack tests PASSED!\n";
    return 0;
}
