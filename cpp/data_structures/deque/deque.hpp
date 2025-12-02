/**
 * @file deque.hpp
 * @brief Double-ended queue data structure implementation
 *
 * A deque allows O(1) insertion and removal at both ends.
 * Implemented using a circular buffer.
 */

#ifndef DSA_DEQUE_HPP
#define DSA_DEQUE_HPP

#include <cstddef>
#include <optional>
#include <utility>

namespace dsa {

template <typename T>
class Deque {
public:
    Deque() : Deque(16) {}

    explicit Deque(std::size_t initial_capacity)
        : data_(new T[initial_capacity + 1]),
          head_(0),
          tail_(0),
          capacity_(initial_capacity + 1) {}

    Deque(const Deque& other)
        : data_(new T[other.capacity_]),
          head_(0),
          tail_(other.size()),
          capacity_(other.capacity_) {
        std::size_t j = 0;
        for (std::size_t i = other.head_; i != other.tail_; i = (i + 1) % other.capacity_) {
            data_[j++] = other.data_[i];
        }
    }

    Deque(Deque&& other) noexcept
        : data_(other.data_), head_(other.head_), tail_(other.tail_), capacity_(other.capacity_) {
        other.data_ = nullptr;
        other.head_ = other.tail_ = other.capacity_ = 0;
    }

    Deque& operator=(const Deque& other) {
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

    Deque& operator=(Deque&& other) noexcept {
        if (this != &other) {
            delete[] data_;
            data_ = other.data_;
            head_ = other.head_;
            tail_ = other.tail_;
            capacity_ = other.capacity_;
            other.data_ = nullptr;
            other.head_ = other.tail_ = other.capacity_ = 0;
        }
        return *this;
    }

    ~Deque() { delete[] data_; }

    void push_front(const T& value) {
        if (full()) resize(capacity_ * 2);
        head_ = (head_ == 0) ? capacity_ - 1 : head_ - 1;
        data_[head_] = value;
    }

    void push_front(T&& value) {
        if (full()) resize(capacity_ * 2);
        head_ = (head_ == 0) ? capacity_ - 1 : head_ - 1;
        data_[head_] = std::move(value);
    }

    void push_back(const T& value) {
        if (full()) resize(capacity_ * 2);
        data_[tail_] = value;
        tail_ = (tail_ + 1) % capacity_;
    }

    void push_back(T&& value) {
        if (full()) resize(capacity_ * 2);
        data_[tail_] = std::move(value);
        tail_ = (tail_ + 1) % capacity_;
    }

    std::optional<T> pop_front() {
        if (empty()) return std::nullopt;
        T value = std::move(data_[head_]);
        head_ = (head_ + 1) % capacity_;
        return value;
    }

    std::optional<T> pop_back() {
        if (empty()) return std::nullopt;
        tail_ = (tail_ == 0) ? capacity_ - 1 : tail_ - 1;
        return std::move(data_[tail_]);
    }

    [[nodiscard]] std::optional<T> front() const {
        if (empty()) return std::nullopt;
        return data_[head_];
    }

    [[nodiscard]] std::optional<T> back() const {
        if (empty()) return std::nullopt;
        std::size_t idx = (tail_ == 0) ? capacity_ - 1 : tail_ - 1;
        return data_[idx];
    }

    [[nodiscard]] std::optional<T> get(std::size_t index) const {
        if (index >= size()) return std::nullopt;
        return data_[(head_ + index) % capacity_];
    }

    [[nodiscard]] bool empty() const noexcept { return head_ == tail_; }

    [[nodiscard]] std::size_t size() const noexcept {
        if (tail_ >= head_) return tail_ - head_;
        return capacity_ - head_ + tail_;
    }

    void clear() noexcept { head_ = tail_ = 0; }

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

#endif // DSA_DEQUE_HPP
