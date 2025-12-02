/**
 * @file singly_linked_list.hpp
 * @brief Singly linked list data structure implementation
 *
 * A linked list where each node points to the next node.
 * Provides O(1) insertion at head, O(n) access and search.
 */

#ifndef DSA_SINGLY_LINKED_LIST_HPP
#define DSA_SINGLY_LINKED_LIST_HPP

#include <cstddef>
#include <optional>
#include <utility>

namespace dsa {

/**
 * A generic singly linked list data structure.
 * @tparam T The type of elements stored in the list
 */
template <typename T>
class SinglyLinkedList {
private:
    struct Node {
        T data;
        Node* next;

        explicit Node(const T& value) : data(value), next(nullptr) {}
        explicit Node(T&& value) : data(std::move(value)), next(nullptr) {}
    };

public:
    /**
     * Construct an empty list.
     */
    SinglyLinkedList() : head_(nullptr), tail_(nullptr), size_(0) {}

    /**
     * Copy constructor.
     */
    SinglyLinkedList(const SinglyLinkedList& other) : head_(nullptr), tail_(nullptr), size_(0) {
        for (Node* curr = other.head_; curr; curr = curr->next) {
            push_back(curr->data);
        }
    }

    /**
     * Move constructor.
     */
    SinglyLinkedList(SinglyLinkedList&& other) noexcept
        : head_(other.head_), tail_(other.tail_), size_(other.size_) {
        other.head_ = nullptr;
        other.tail_ = nullptr;
        other.size_ = 0;
    }

    /**
     * Copy assignment operator.
     */
    SinglyLinkedList& operator=(const SinglyLinkedList& other) {
        if (this != &other) {
            clear();
            for (Node* curr = other.head_; curr; curr = curr->next) {
                push_back(curr->data);
            }
        }
        return *this;
    }

    /**
     * Move assignment operator.
     */
    SinglyLinkedList& operator=(SinglyLinkedList&& other) noexcept {
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

    /**
     * Destructor.
     */
    ~SinglyLinkedList() {
        clear();
    }

    /**
     * Add an element to the front.
     * @param value The value to add
     */
    void push_front(const T& value) {
        Node* node = new Node(value);
        node->next = head_;
        head_ = node;
        if (!tail_) {
            tail_ = head_;
        }
        ++size_;
    }

    /**
     * Add an element to the front (move version).
     * @param value The value to add
     */
    void push_front(T&& value) {
        Node* node = new Node(std::move(value));
        node->next = head_;
        head_ = node;
        if (!tail_) {
            tail_ = head_;
        }
        ++size_;
    }

    /**
     * Add an element to the back.
     * @param value The value to add
     */
    void push_back(const T& value) {
        Node* node = new Node(value);
        if (tail_) {
            tail_->next = node;
        } else {
            head_ = node;
        }
        tail_ = node;
        ++size_;
    }

    /**
     * Add an element to the back (move version).
     * @param value The value to add
     */
    void push_back(T&& value) {
        Node* node = new Node(std::move(value));
        if (tail_) {
            tail_->next = node;
        } else {
            head_ = node;
        }
        tail_ = node;
        ++size_;
    }

    /**
     * Remove and return the front element.
     * @return The front element, or std::nullopt if empty
     */
    std::optional<T> pop_front() {
        if (!head_) {
            return std::nullopt;
        }
        Node* node = head_;
        T value = std::move(node->data);
        head_ = head_->next;
        if (!head_) {
            tail_ = nullptr;
        }
        delete node;
        --size_;
        return value;
    }

    /**
     * Return the front element without removing it.
     * @return The front element, or std::nullopt if empty
     */
    [[nodiscard]] std::optional<T> front() const {
        if (!head_) {
            return std::nullopt;
        }
        return head_->data;
    }

    /**
     * Return the back element without removing it.
     * @return The back element, or std::nullopt if empty
     */
    [[nodiscard]] std::optional<T> back() const {
        if (!tail_) {
            return std::nullopt;
        }
        return tail_->data;
    }

    /**
     * Get element at index.
     * @param index Position to access
     * @return The element, or std::nullopt if index is out of bounds
     */
    [[nodiscard]] std::optional<T> get(std::size_t index) const {
        Node* node = get_node(index);
        if (!node) {
            return std::nullopt;
        }
        return node->data;
    }

    /**
     * Insert at a specific index.
     * @param index Position to insert at
     * @param value The value to insert
     * @return true if successful, false if index is out of bounds
     */
    bool insert(std::size_t index, const T& value) {
        if (index > size_) {
            return false;
        }
        if (index == 0) {
            push_front(value);
            return true;
        }
        if (index == size_) {
            push_back(value);
            return true;
        }
        Node* prev = get_node(index - 1);
        Node* node = new Node(value);
        node->next = prev->next;
        prev->next = node;
        ++size_;
        return true;
    }

    /**
     * Remove element at a specific index.
     * @param index Position to remove
     * @return The removed element, or std::nullopt if index is out of bounds
     */
    std::optional<T> remove(std::size_t index) {
        if (index >= size_) {
            return std::nullopt;
        }
        if (index == 0) {
            return pop_front();
        }
        Node* prev = get_node(index - 1);
        Node* node = prev->next;
        T value = std::move(node->data);
        prev->next = node->next;
        if (node == tail_) {
            tail_ = prev;
        }
        delete node;
        --size_;
        return value;
    }

    /**
     * Find the first occurrence of a value.
     * @param value The value to find
     * @return Index of the value, or std::nullopt if not found
     */
    [[nodiscard]] std::optional<std::size_t> find(const T& value) const {
        std::size_t index = 0;
        for (Node* curr = head_; curr; curr = curr->next) {
            if (curr->data == value) {
                return index;
            }
            ++index;
        }
        return std::nullopt;
    }

    /**
     * Check if the list contains a value.
     * @param value The value to check
     * @return true if found, false otherwise
     */
    [[nodiscard]] bool contains(const T& value) const {
        return find(value).has_value();
    }

    /**
     * Reverse the list in place.
     */
    void reverse() {
        Node* prev = nullptr;
        Node* curr = head_;
        tail_ = head_;
        while (curr) {
            Node* next = curr->next;
            curr->next = prev;
            prev = curr;
            curr = next;
        }
        head_ = prev;
    }

    /**
     * Check if the list is empty.
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
     * Remove all elements.
     */
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
        if (index >= size_) {
            return nullptr;
        }
        Node* curr = head_;
        for (std::size_t i = 0; i < index; ++i) {
            curr = curr->next;
        }
        return curr;
    }
};

} // namespace dsa

#endif // DSA_SINGLY_LINKED_LIST_HPP
