/**
 * @file hash_table.hpp
 * @brief Hash table data structure implementation using open addressing
 *
 * A key-value store with O(1) average case operations.
 * Uses linear probing for collision resolution.
 */

#ifndef DSA_HASH_TABLE_HPP
#define DSA_HASH_TABLE_HPP

#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <utility>

namespace dsa {

template <typename K, typename V, typename Hash = std::hash<K>>
class HashTable {
private:
    enum class State { EMPTY, OCCUPIED, DELETED };

    struct Entry {
        K key;
        V value;
        State state;

        Entry() : state(State::EMPTY) {}
    };

public:
    HashTable() : HashTable(16) {}

    explicit HashTable(std::size_t initial_capacity)
        : entries_(new Entry[initial_capacity]),
          capacity_(initial_capacity),
          size_(0),
          hasher_() {}

    HashTable(const HashTable& other)
        : entries_(new Entry[other.capacity_]),
          capacity_(other.capacity_),
          size_(other.size_),
          hasher_(other.hasher_) {
        for (std::size_t i = 0; i < capacity_; ++i) {
            entries_[i] = other.entries_[i];
        }
    }

    HashTable(HashTable&& other) noexcept
        : entries_(other.entries_),
          capacity_(other.capacity_),
          size_(other.size_),
          hasher_(std::move(other.hasher_)) {
        other.entries_ = nullptr;
        other.capacity_ = 0;
        other.size_ = 0;
    }

    HashTable& operator=(const HashTable& other) {
        if (this != &other) {
            delete[] entries_;
            entries_ = new Entry[other.capacity_];
            capacity_ = other.capacity_;
            size_ = other.size_;
            hasher_ = other.hasher_;
            for (std::size_t i = 0; i < capacity_; ++i) {
                entries_[i] = other.entries_[i];
            }
        }
        return *this;
    }

    HashTable& operator=(HashTable&& other) noexcept {
        if (this != &other) {
            delete[] entries_;
            entries_ = other.entries_;
            capacity_ = other.capacity_;
            size_ = other.size_;
            hasher_ = std::move(other.hasher_);
            other.entries_ = nullptr;
            other.capacity_ = 0;
            other.size_ = 0;
        }
        return *this;
    }

    ~HashTable() { delete[] entries_; }

    void insert(const K& key, const V& value) {
        if (load_factor() >= 0.7) {
            resize(capacity_ * 2);
        }
        std::size_t idx = find_slot(key);
        if (entries_[idx].state != State::OCCUPIED) {
            ++size_;
        }
        entries_[idx].key = key;
        entries_[idx].value = value;
        entries_[idx].state = State::OCCUPIED;
    }

    void insert(const K& key, V&& value) {
        if (load_factor() >= 0.7) {
            resize(capacity_ * 2);
        }
        std::size_t idx = find_slot(key);
        if (entries_[idx].state != State::OCCUPIED) {
            ++size_;
        }
        entries_[idx].key = key;
        entries_[idx].value = std::move(value);
        entries_[idx].state = State::OCCUPIED;
    }

    [[nodiscard]] std::optional<V> get(const K& key) const {
        auto idx = find_key(key);
        if (!idx.has_value()) return std::nullopt;
        return entries_[idx.value()].value;
    }

    bool remove(const K& key) {
        auto idx = find_key(key);
        if (!idx.has_value()) return false;
        entries_[idx.value()].state = State::DELETED;
        --size_;
        return true;
    }

    [[nodiscard]] bool contains(const K& key) const {
        return find_key(key).has_value();
    }

    V& operator[](const K& key) {
        if (load_factor() >= 0.7) {
            resize(capacity_ * 2);
        }
        std::size_t idx = find_slot(key);
        if (entries_[idx].state != State::OCCUPIED) {
            entries_[idx].key = key;
            entries_[idx].value = V{};
            entries_[idx].state = State::OCCUPIED;
            ++size_;
        }
        return entries_[idx].value;
    }

    [[nodiscard]] bool empty() const noexcept { return size_ == 0; }
    [[nodiscard]] std::size_t size() const noexcept { return size_; }

    void clear() {
        for (std::size_t i = 0; i < capacity_; ++i) {
            entries_[i].state = State::EMPTY;
        }
        size_ = 0;
    }

private:
    Entry* entries_;
    std::size_t capacity_;
    std::size_t size_;
    Hash hasher_;

    [[nodiscard]] double load_factor() const {
        return static_cast<double>(size_) / static_cast<double>(capacity_);
    }

    [[nodiscard]] std::size_t hash(const K& key) const {
        return hasher_(key) % capacity_;
    }

    [[nodiscard]] std::size_t find_slot(const K& key) const {
        std::size_t idx = hash(key);
        std::size_t first_deleted = capacity_;

        while (entries_[idx].state != State::EMPTY) {
            if (entries_[idx].state == State::OCCUPIED && entries_[idx].key == key) {
                return idx;
            }
            if (entries_[idx].state == State::DELETED && first_deleted == capacity_) {
                first_deleted = idx;
            }
            idx = (idx + 1) % capacity_;
        }

        return (first_deleted != capacity_) ? first_deleted : idx;
    }

    [[nodiscard]] std::optional<std::size_t> find_key(const K& key) const {
        std::size_t idx = hash(key);
        std::size_t start = idx;

        do {
            if (entries_[idx].state == State::EMPTY) {
                return std::nullopt;
            }
            if (entries_[idx].state == State::OCCUPIED && entries_[idx].key == key) {
                return idx;
            }
            idx = (idx + 1) % capacity_;
        } while (idx != start);

        return std::nullopt;
    }

    void resize(std::size_t new_capacity) {
        Entry* old_entries = entries_;
        std::size_t old_capacity = capacity_;

        entries_ = new Entry[new_capacity];
        capacity_ = new_capacity;
        size_ = 0;

        for (std::size_t i = 0; i < old_capacity; ++i) {
            if (old_entries[i].state == State::OCCUPIED) {
                insert(old_entries[i].key, std::move(old_entries[i].value));
            }
        }

        delete[] old_entries;
    }
};

} // namespace dsa

#endif // DSA_HASH_TABLE_HPP
