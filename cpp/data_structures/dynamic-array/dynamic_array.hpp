/**
 * @file dynamic_array.hpp
 * @brief Dynamic array (vector) data structure implementation
 *
 * A resizable array that grows automatically when capacity is exceeded.
 * Provides O(1) amortized push_back and O(1) random access.
 */

#ifndef DSA_DYNAMIC_ARRAY_HPP
#define DSA_DYNAMIC_ARRAY_HPP

#include <cstddef>
#include <optional>
#include <stdexcept>
#include <utility>

namespace dsa {

/**
 * A generic dynamic array data structure.
 * @tparam T The type of elements stored in the array
 */
template <typename T>
class DynamicArray {
public:
    /**
     * Construct an empty dynamic array with default initial capacity.
     */
    DynamicArray() : DynamicArray(16) {}

    /**
     * Construct an empty dynamic array with specified initial capacity.
     * @param initial_capacity Initial capacity for the underlying array
     */
    explicit DynamicArray(std::size_t initial_capacity)
        : data_(initial_capacity > 0 ? new T[initial_capacity] : nullptr),
          size_(0),
          capacity_(initial_capacity) {}

    /**
     * Copy constructor.
     */
    DynamicArray(const DynamicArray& other)
        : data_(other.capacity_ > 0 ? new T[other.capacity_] : nullptr),
          size_(other.size_),
          capacity_(other.capacity_) {
        for (std::size_t i = 0; i < size_; ++i) {
            data_[i] = other.data_[i];
        }
    }

    /**
     * Move constructor.
     */
    DynamicArray(DynamicArray&& other) noexcept
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
    DynamicArray& operator=(const DynamicArray& other) {
        if (this != &other) {
            delete[] data_;
            data_ = other.capacity_ > 0 ? new T[other.capacity_] : nullptr;
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
    DynamicArray& operator=(DynamicArray&& other) noexcept {
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
    ~DynamicArray() {
        delete[] data_;
    }

    /**
     * Add an element to the end.
     * @param value The value to add
     */
    void push_back(const T& value) {
        ensure_capacity(size_ + 1);
        data_[size_++] = value;
    }

    /**
     * Add an element to the end (move version).
     * @param value The value to add
     */
    void push_back(T&& value) {
        ensure_capacity(size_ + 1);
        data_[size_++] = std::move(value);
    }

    /**
     * Remove and return the last element.
     * @return The last element, or std::nullopt if empty
     */
    std::optional<T> pop_back() {
        if (size_ == 0) {
            return std::nullopt;
        }
        return std::move(data_[--size_]);
    }

    /**
     * Insert an element at a specific index.
     * @param index Position to insert at
     * @param value The value to insert
     * @return true if successful, false if index is out of bounds
     */
    bool insert(std::size_t index, const T& value) {
        if (index > size_) {
            return false;
        }
        ensure_capacity(size_ + 1);
        for (std::size_t i = size_; i > index; --i) {
            data_[i] = std::move(data_[i - 1]);
        }
        data_[index] = value;
        ++size_;
        return true;
    }

    /**
     * Remove an element at a specific index.
     * @param index Position to remove from
     * @return The removed element, or std::nullopt if index is out of bounds
     */
    std::optional<T> remove(std::size_t index) {
        if (index >= size_) {
            return std::nullopt;
        }
        T value = std::move(data_[index]);
        for (std::size_t i = index; i < size_ - 1; ++i) {
            data_[i] = std::move(data_[i + 1]);
        }
        --size_;
        return value;
    }

    /**
     * Access element at index (bounds checked).
     * @param index Position to access
     * @return The element, or std::nullopt if index is out of bounds
     */
    [[nodiscard]] std::optional<T> get(std::size_t index) const {
        if (index >= size_) {
            return std::nullopt;
        }
        return data_[index];
    }

    /**
     * Set element at index.
     * @param index Position to set
     * @param value The value to set
     * @return true if successful, false if index is out of bounds
     */
    bool set(std::size_t index, const T& value) {
        if (index >= size_) {
            return false;
        }
        data_[index] = value;
        return true;
    }

    /**
     * Access element at index (unchecked).
     * @param index Position to access
     * @return Reference to the element
     */
    T& operator[](std::size_t index) {
        return data_[index];
    }

    /**
     * Access element at index (unchecked, const).
     * @param index Position to access
     * @return Const reference to the element
     */
    const T& operator[](std::size_t index) const {
        return data_[index];
    }

    /**
     * Find the first occurrence of a value.
     * @param value The value to find
     * @return Index of the value, or std::nullopt if not found
     */
    [[nodiscard]] std::optional<std::size_t> find(const T& value) const {
        for (std::size_t i = 0; i < size_; ++i) {
            if (data_[i] == value) {
                return i;
            }
        }
        return std::nullopt;
    }

    /**
     * Check if the array is empty.
     * @return true if empty, false otherwise
     */
    [[nodiscard]] bool empty() const noexcept {
        return size_ == 0;
    }

    /**
     * Get the number of elements.
     * @return Number of elements
     */
    [[nodiscard]] std::size_t size() const noexcept {
        return size_;
    }

    /**
     * Get the current capacity.
     * @return Capacity
     */
    [[nodiscard]] std::size_t capacity() const noexcept {
        return capacity_;
    }

    /**
     * Remove all elements.
     */
    void clear() noexcept {
        size_ = 0;
    }

    /**
     * Reserve capacity for at least n elements.
     * @param n Minimum capacity
     */
    void reserve(std::size_t n) {
        if (n > capacity_) {
            resize(n);
        }
    }

    // Iterator support
    T* begin() noexcept { return data_; }
    T* end() noexcept { return data_ + size_; }
    const T* begin() const noexcept { return data_; }
    const T* end() const noexcept { return data_ + size_; }

private:
    T* data_;
    std::size_t size_;
    std::size_t capacity_;

    void ensure_capacity(std::size_t required) {
        if (required > capacity_) {
            resize(capacity_ == 0 ? 1 : capacity_ * 2);
        }
    }

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

#endif // DSA_DYNAMIC_ARRAY_HPP
