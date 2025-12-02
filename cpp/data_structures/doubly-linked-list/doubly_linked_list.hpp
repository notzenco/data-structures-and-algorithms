/**
 * @file doubly_linked_list.hpp
 * @brief Doubly linked list data structure implementation
 *
 * A linked list where each node points to both next and previous nodes.
 * Provides O(1) insertion/deletion at both ends.
 */

#ifndef DSA_DOUBLY_LINKED_LIST_HPP
#define DSA_DOUBLY_LINKED_LIST_HPP

#include <cstddef>
#include <optional>
#include <utility>

namespace dsa {

/**
 * A generic doubly linked list data structure.
 * @tparam T The type of elements stored in the list
 */
template <typename T>
class DoublyLinkedList {
private:
    struct Node {
        T data;
        Node* prev;
        Node* next;

        explicit Node(const T& value) : data(value), prev(nullptr), next(nullptr) {}
        explicit Node(T&& value) : data(std::move(value)), prev(nullptr), next(nullptr) {}
    };

public:
    DoublyLinkedList() : head_(nullptr), tail_(nullptr), size_(0) {}

    DoublyLinkedList(const DoublyLinkedList& other) : head_(nullptr), tail_(nullptr), size_(0) {
        for (Node* curr = other.head_; curr; curr = curr->next) {
            push_back(curr->data);
        }
    }

    DoublyLinkedList(DoublyLinkedList&& other) noexcept
        : head_(other.head_), tail_(other.tail_), size_(other.size_) {
        other.head_ = nullptr;
        other.tail_ = nullptr;
        other.size_ = 0;
    }

    DoublyLinkedList& operator=(const DoublyLinkedList& other) {
        if (this != &other) {
            clear();
            for (Node* curr = other.head_; curr; curr = curr->next) {
                push_back(curr->data);
            }
        }
        return *this;
    }

    DoublyLinkedList& operator=(DoublyLinkedList&& other) noexcept {
        if (this != &other) {
            clear();
            head_ = other.head_;
            tail_ = other.tail_;
            size_ = other.size_;
            other.head_ = nullptr;
            other.tail_ = nullptr;
            other.size_ = 0;
        }
        return *this;
    }

    ~DoublyLinkedList() {
        clear();
    }

    void push_front(const T& value) {
        Node* node = new Node(value);
        node->next = head_;
        if (head_) {
            head_->prev = node;
        } else {
            tail_ = node;
        }
        head_ = node;
        ++size_;
    }

    void push_front(T&& value) {
        Node* node = new Node(std::move(value));
        node->next = head_;
        if (head_) {
            head_->prev = node;
        } else {
            tail_ = node;
        }
        head_ = node;
        ++size_;
    }

    void push_back(const T& value) {
        Node* node = new Node(value);
        node->prev = tail_;
        if (tail_) {
            tail_->next = node;
        } else {
            head_ = node;
        }
        tail_ = node;
        ++size_;
    }

    void push_back(T&& value) {
        Node* node = new Node(std::move(value));
        node->prev = tail_;
        if (tail_) {
            tail_->next = node;
        } else {
            head_ = node;
        }
        tail_ = node;
        ++size_;
    }

    std::optional<T> pop_front() {
        if (!head_) {
            return std::nullopt;
        }
        Node* node = head_;
        T value = std::move(node->data);
        head_ = head_->next;
        if (head_) {
            head_->prev = nullptr;
        } else {
            tail_ = nullptr;
        }
        delete node;
        --size_;
        return value;
    }

    std::optional<T> pop_back() {
        if (!tail_) {
            return std::nullopt;
        }
        Node* node = tail_;
        T value = std::move(node->data);
        tail_ = tail_->prev;
        if (tail_) {
            tail_->next = nullptr;
        } else {
            head_ = nullptr;
        }
        delete node;
        --size_;
        return value;
    }

    [[nodiscard]] std::optional<T> front() const {
        if (!head_) return std::nullopt;
        return head_->data;
    }

    [[nodiscard]] std::optional<T> back() const {
        if (!tail_) return std::nullopt;
        return tail_->data;
    }

    [[nodiscard]] std::optional<T> get(std::size_t index) const {
        Node* node = get_node(index);
        if (!node) return std::nullopt;
        return node->data;
    }

    bool insert(std::size_t index, const T& value) {
        if (index > size_) return false;
        if (index == 0) { push_front(value); return true; }
        if (index == size_) { push_back(value); return true; }

        Node* next_node = get_node(index);
        Node* prev_node = next_node->prev;
        Node* node = new Node(value);
        node->prev = prev_node;
        node->next = next_node;
        prev_node->next = node;
        next_node->prev = node;
        ++size_;
        return true;
    }

    std::optional<T> remove(std::size_t index) {
        if (index >= size_) return std::nullopt;
        if (index == 0) return pop_front();
        if (index == size_ - 1) return pop_back();

        Node* node = get_node(index);
        T value = std::move(node->data);
        node->prev->next = node->next;
        node->next->prev = node->prev;
        delete node;
        --size_;
        return value;
    }

    [[nodiscard]] std::optional<std::size_t> find(const T& value) const {
        std::size_t index = 0;
        for (Node* curr = head_; curr; curr = curr->next) {
            if (curr->data == value) return index;
            ++index;
        }
        return std::nullopt;
    }

    [[nodiscard]] bool contains(const T& value) const {
        return find(value).has_value();
    }

    void reverse() {
        Node* curr = head_;
        std::swap(head_, tail_);
        while (curr) {
            std::swap(curr->prev, curr->next);
            curr = curr->prev;
        }
    }

    [[nodiscard]] bool empty() const noexcept { return size_ == 0; }
    [[nodiscard]] std::size_t size() const noexcept { return size_; }

    void clear() {
        while (head_) {
            Node* next = head_->next;
            delete head_;
            head_ = next;
        }
        tail_ = nullptr;
        size_ = 0;
    }

private:
    Node* head_;
    Node* tail_;
    std::size_t size_;

    Node* get_node(std::size_t index) const {
        if (index >= size_) return nullptr;
        Node* curr;
        if (index < size_ / 2) {
            curr = head_;
            for (std::size_t i = 0; i < index; ++i) curr = curr->next;
        } else {
            curr = tail_;
            for (std::size_t i = size_ - 1; i > index; --i) curr = curr->prev;
        }
        return curr;
    }
};

} // namespace dsa

#endif // DSA_DOUBLY_LINKED_LIST_HPP
