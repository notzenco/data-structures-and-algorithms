/**
 * @file stack.hpp
 * @brief Stack data structure implementation
 *
 * A LIFO (Last In, First Out) data structure implemented using a dynamic array.
 * Provides O(1) push, pop, and peek operations.
 */

#ifndef DSA_STACK_HPP
#define DSA_STACK_HPP

#include <cstddef>
#include <optional>
#include <stdexcept>
#include <utility>

namespace dsa {

/**
 * A generic stack data structure.
 * @tparam T The type of elements stored in the stack
 */
template <typename T>
class Stack {
public:
    /**
     * Construct an empty stack with default initial capacity.
     */
    Stack() : Stack(16) {}

    /**
     * Construct an empty stack with specified initial capacity.
     * @param initial_capacity Initial capacity for the underlying array
     */
    explicit Stack(std::size_t initial_capacity)
        : data_(new T[initial_capacity]),
          size_(0),
          capacity_(initial_capacity) {}

    /**
     * Copy constructor.
     */
    Stack(const Stack& other)
        : data_(new T[other.capacity_]),
          size_(other.size_),
          capacity_(other.capacity_) {
        for (std::size_t i = 0; i < size_; ++i) {
            data_[i] = other.data_[i];
        }
    }

    /**
     * Move constructor.
     */
    Stack(Stack&& other) noexcept
        : data_(other.data_),
          size_(other.size_),
          capacity_(other.capacity_) {
        other.data_ = nullptr;
        other.size_ = 0;
        other.capacity_ = 0;
    }

    /**
     * Copy assignment operator.
     */
    Stack& operator=(const Stack& other) {
        if (this != &other) {
            delete[] data_;
            data_ = new T[other.capacity_];
            size_ = other.size_;
            capacity_ = other.capacity_;
            for (std::size_t i = 0; i < size_; ++i) {
                data_[i] = other.data_[i];
            }
        }
        return *this;
    }

    /**
     * Move assignment operator.
     */
    Stack& operator=(Stack&& other) noexcept {
        if (this != &other) {
            delete[] data_;
            data_ = other.data_;
            size_ = other.size_;
            capacity_ = other.capacity_;
            other.data_ = nullptr;
            other.size_ = 0;
            other.capacity_ = 0;
        }
        return *this;
    }

    /**
     * Destructor.
     */
    ~Stack() {
        delete[] data_;
    }

    /**
     * Push an element onto the stack.
     * @param value The value to push
     */
    void push(const T& value) {
        if (size_ >= capacity_) {
            resize(capacity_ == 0 ? 1 : capacity_ * 2);
        }
        data_[size_++] = value;
    }

    /**
     * Push an element onto the stack (move version).
     * @param value The value to push
     */
    void push(T&& value) {
        if (size_ >= capacity_) {
            resize(capacity_ == 0 ? 1 : capacity_ * 2);
        }
        data_[size_++] = std::move(value);
    }

    /**
     * Remove and return the top element.
     * @return The top element, or std::nullopt if empty
     */
    std::optional<T> pop() {
        if (size_ == 0) {
            return std::nullopt;
        }
        return std::move(data_[--size_]);
    }

    /**
     * Return the top element without removing it.
     * @return The top element, or std::nullopt if empty
     */
    [[nodiscard]] std::optional<T> peek() const {
        if (size_ == 0) {
            return std::nullopt;
        }
        return data_[size_ - 1];
    }

    /**
     * Check if the stack is empty.
     * @return true if empty, false otherwise
     */
    [[nodiscard]] bool empty() const noexcept {
        return size_ == 0;
    }

    /**
     * Get the number of elements in the stack.
     * @return Number of elements
     */
    [[nodiscard]] std::size_t size() const noexcept {
        return size_;
    }

    /**
     * Remove all elements from the stack.
     */
    void clear() noexcept {
        size_ = 0;
    }

private:
    T* data_;
    std::size_t size_;
    std::size_t capacity_;

    void resize(std::size_t new_capacity) {
        T* new_data = new T[new_capacity];
        for (std::size_t i = 0; i < size_; ++i) {
            new_data[i] = std::move(data_[i]);
        }
        delete[] data_;
        data_ = new_data;
        capacity_ = new_capacity;
    }
};

} // namespace dsa

#endif // DSA_STACK_HPP
