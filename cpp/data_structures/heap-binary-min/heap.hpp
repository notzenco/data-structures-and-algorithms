/**
 * @file heap.hpp
 * @brief Binary min heap data structure implementation
 *
 * A complete binary tree where each node is smaller than its children.
 * Provides O(log n) insert and extract_min, O(1) peek.
 */

#ifndef DSA_HEAP_HPP
#define DSA_HEAP_HPP

#include <cstddef>
#include <optional>
#include <utility>
#include <vector>

namespace dsa {

template <typename T>
class MinHeap {
public:
    MinHeap() = default;

    explicit MinHeap(std::size_t initial_capacity) {
        data_.reserve(initial_capacity);
    }

    MinHeap(const MinHeap&) = default;
    MinHeap(MinHeap&&) noexcept = default;
    MinHeap& operator=(const MinHeap&) = default;
    MinHeap& operator=(MinHeap&&) noexcept = default;
    ~MinHeap() = default;

    void insert(const T& value) {
        data_.push_back(value);
        sift_up(data_.size() - 1);
    }

    void insert(T&& value) {
        data_.push_back(std::move(value));
        sift_up(data_.size() - 1);
    }

    std::optional<T> extract_min() {
        if (data_.empty()) return std::nullopt;

        T min_val = std::move(data_[0]);
        data_[0] = std::move(data_.back());
        data_.pop_back();

        if (!data_.empty()) {
            sift_down(0);
        }

        return min_val;
    }

    [[nodiscard]] std::optional<T> peek() const {
        if (data_.empty()) return std::nullopt;
        return data_[0];
    }

    void decrease_key(std::size_t index, const T& new_value) {
        if (index >= data_.size()) return;
        if (new_value >= data_[index]) return;
        data_[index] = new_value;
        sift_up(index);
    }

    [[nodiscard]] bool empty() const noexcept { return data_.empty(); }
    [[nodiscard]] std::size_t size() const noexcept { return data_.size(); }

    void clear() { data_.clear(); }

    static MinHeap heapify(const std::vector<T>& values) {
        MinHeap heap;
        heap.data_ = values;
        if (heap.data_.size() > 1) {
            for (std::size_t i = heap.data_.size() / 2; i > 0; --i) {
                heap.sift_down(i - 1);
            }
            heap.sift_down(0);
        }
        return heap;
    }

private:
    std::vector<T> data_;

    static std::size_t parent(std::size_t i) { return (i - 1) / 2; }
    static std::size_t left_child(std::size_t i) { return 2 * i + 1; }
    static std::size_t right_child(std::size_t i) { return 2 * i + 2; }

    void sift_up(std::size_t index) {
        while (index > 0 && data_[index] < data_[parent(index)]) {
            std::swap(data_[index], data_[parent(index)]);
            index = parent(index);
        }
    }

    void sift_down(std::size_t index) {
        std::size_t min_idx = index;
        std::size_t left = left_child(index);
        std::size_t right = right_child(index);

        if (left < data_.size() && data_[left] < data_[min_idx]) {
            min_idx = left;
        }
        if (right < data_.size() && data_[right] < data_[min_idx]) {
            min_idx = right;
        }

        if (min_idx != index) {
            std::swap(data_[index], data_[min_idx]);
            sift_down(min_idx);
        }
    }
};

} // namespace dsa

#endif // DSA_HEAP_HPP
