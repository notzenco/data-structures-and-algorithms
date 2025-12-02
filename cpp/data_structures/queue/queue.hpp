/**
 * @file queue.hpp
 * @brief Queue data structure implementation
 *
 * A FIFO (First In, First Out) data structure implemented using a circular buffer.
 * Provides O(1) enqueue, dequeue, and front operations.
 */

#ifndef DSA_QUEUE_HPP
#define DSA_QUEUE_HPP

#include <cstddef>
#include <optional>
#include <utility>

namespace dsa {

/**
 * A generic queue data structure.
 * @tparam T The type of elements stored in the queue
 */
template <typename T>
class Queue {
public:
    /**
     * Construct an empty queue with default initial capacity.
     */
    Queue() : Queue(16) {}

    /**
     * Construct an empty queue with specified initial capacity.
     * @param initial_capacity Initial capacity for the underlying array
     */
    explicit Queue(std::size_t initial_capacity)
        : data_(new T[initial_capacity + 1]),
          head_(0),
          tail_(0),
          capacity_(initial_capacity + 1) {}

    /**
     * Copy constructor.
     */
    Queue(const Queue& other)
        : data_(new T[other.capacity_]),
          head_(0),
          tail_(other.size()),
          capacity_(other.capacity_) {
        std::size_t j = 0;
        for (std::size_t i = other.head_; i != other.tail_; i = (i + 1) % other.capacity_) {
            data_[j++] = other.data_[i];
        }
    }

    /**
     * Move constructor.
     */
    Queue(Queue&& other) noexcept
        : data_(other.data_),
          head_(other.head_),
          tail_(other.tail_),
          capacity_(other.capacity_) {
        other.data_ = nullptr;
        other.head_ = 0;
        other.tail_ = 0;
        other.capacity_ = 0;
    }

    /**
     * Copy assignment operator.
     */
    Queue& operator=(const Queue& other) {
        if (this != &other) {
            delete[] data_;
            data_ = new T[other.capacity_];
            head_ = 0;
            tail_ = other.size();
            capacity_ = other.capacity_;
            std::size_t j = 0;
            for (std::size_t i = other.head_; i != other.tail_; i = (i + 1) % other.capacity_) {
                data_[j++] = other.data_[i];
            }
        }
        return *this;
    }

    /**
     * Move assignment operator.
     */
    Queue& operator=(Queue&& other) noexcept {
        if (this != &other) {
            delete[] data_;
            data_ = other.data_;
            head_ = other.head_;
            tail_ = other.tail_;
            capacity_ = other.capacity_;
            other.data_ = nullptr;
            other.head_ = 0;
            other.tail_ = 0;
            other.capacity_ = 0;
        }
        return *this;
    }

    /**
     * Destructor.
     */
    ~Queue() {
        delete[] data_;
    }

    /**
     * Add an element to the back of the queue.
     * @param value The value to enqueue
     */
    void enqueue(const T& value) {
        if (full()) {
            resize(capacity_ * 2);
        }
        data_[tail_] = value;
        tail_ = (tail_ + 1) % capacity_;
    }

    /**
     * Add an element to the back of the queue (move version).
     * @param value The value to enqueue
     */
    void enqueue(T&& value) {
        if (full()) {
            resize(capacity_ * 2);
        }
        data_[tail_] = std::move(value);
        tail_ = (tail_ + 1) % capacity_;
    }

    /**
     * Remove and return the front element.
     * @return The front element, or std::nullopt if empty
     */
    std::optional<T> dequeue() {
        if (empty()) {
            return std::nullopt;
        }
        T value = std::move(data_[head_]);
        head_ = (head_ + 1) % capacity_;
        return value;
    }

    /**
     * Return the front element without removing it.
     * @return The front element, or std::nullopt if empty
     */
    [[nodiscard]] std::optional<T> front() const {
        if (empty()) {
            return std::nullopt;
        }
        return data_[head_];
    }

    /**
     * Return the back element without removing it.
     * @return The back element, or std::nullopt if empty
     */
    [[nodiscard]] std::optional<T> back() const {
        if (empty()) {
            return std::nullopt;
        }
        std::size_t back_idx = (tail_ == 0) ? capacity_ - 1 : tail_ - 1;
        return data_[back_idx];
    }

    /**
     * Check if the queue is empty.
     * @return true if empty, false otherwise
     */
    [[nodiscard]] bool empty() const noexcept {
        return head_ == tail_;
    }

    /**
     * Get the number of elements in the queue.
     * @return Number of elements
     */
    [[nodiscard]] std::size_t size() const noexcept {
        if (tail_ >= head_) {
            return tail_ - head_;
        }
        return capacity_ - head_ + tail_;
    }

    /**
     * Remove all elements from the queue.
     */
    void clear() noexcept {
        head_ = 0;
        tail_ = 0;
    }

private:
    T* data_;
    std::size_t head_;
    std::size_t tail_;
    std::size_t capacity_;

    [[nodiscard]] bool full() const noexcept {
        return (tail_ + 1) % capacity_ == head_;
    }

    void resize(std::size_t new_capacity) {
        T* new_data = new T[new_capacity];
        std::size_t new_size = size();
        std::size_t j = 0;
        for (std::size_t i = head_; i != tail_; i = (i + 1) % capacity_) {
            new_data[j++] = std::move(data_[i]);
        }
        delete[] data_;
        data_ = new_data;
        head_ = 0;
        tail_ = new_size;
        capacity_ = new_capacity;
    }
};

} // namespace dsa

#endif // DSA_QUEUE_HPP
