/**
 * @file bst.hpp
 * @brief Binary Search Tree data structure implementation
 *
 * A binary tree where left children are smaller and right children are larger.
 * Provides O(log n) average case operations.
 */

#ifndef DSA_BST_HPP
#define DSA_BST_HPP

#include <cstddef>
#include <functional>
#include <optional>
#include <utility>
#include <vector>

namespace dsa {

template <typename T>
class BST {
private:
    struct Node {
        T data;
        Node* left;
        Node* right;

        explicit Node(const T& value) : data(value), left(nullptr), right(nullptr) {}
        explicit Node(T&& value) : data(std::move(value)), left(nullptr), right(nullptr) {}
    };

public:
    BST() : root_(nullptr), size_(0) {}

    BST(const BST& other) : root_(nullptr), size_(0) {
        copy_tree(other.root_);
    }

    BST(BST&& other) noexcept : root_(other.root_), size_(other.size_) {
        other.root_ = nullptr;
        other.size_ = 0;
    }

    BST& operator=(const BST& other) {
        if (this != &other) {
            clear();
            copy_tree(other.root_);
        }
        return *this;
    }

    BST& operator=(BST&& other) noexcept {
        if (this != &other) {
            clear();
            root_ = other.root_;
            size_ = other.size_;
            other.root_ = nullptr;
            other.size_ = 0;
        }
        return *this;
    }

    ~BST() { clear(); }

    void insert(const T& value) {
        root_ = insert_node(root_, value);
        ++size_;
    }

    void insert(T&& value) {
        root_ = insert_node(root_, std::move(value));
        ++size_;
    }

    bool remove(const T& value) {
        if (!contains(value)) return false;
        root_ = remove_node(root_, value);
        --size_;
        return true;
    }

    [[nodiscard]] bool contains(const T& value) const {
        return find_node(root_, value) != nullptr;
    }

    [[nodiscard]] std::optional<T> find(const T& value) const {
        Node* node = find_node(root_, value);
        if (!node) return std::nullopt;
        return node->data;
    }

    [[nodiscard]] std::optional<T> min() const {
        if (!root_) return std::nullopt;
        return find_min(root_)->data;
    }

    [[nodiscard]] std::optional<T> max() const {
        if (!root_) return std::nullopt;
        return find_max(root_)->data;
    }

    [[nodiscard]] std::vector<T> inorder() const {
        std::vector<T> result;
        inorder_traverse(root_, result);
        return result;
    }

    [[nodiscard]] std::vector<T> preorder() const {
        std::vector<T> result;
        preorder_traverse(root_, result);
        return result;
    }

    [[nodiscard]] std::vector<T> postorder() const {
        std::vector<T> result;
        postorder_traverse(root_, result);
        return result;
    }

    [[nodiscard]] std::size_t height() const {
        return tree_height(root_);
    }

    [[nodiscard]] bool empty() const noexcept { return size_ == 0; }
    [[nodiscard]] std::size_t size() const noexcept { return size_; }

    void clear() {
        destroy_tree(root_);
        root_ = nullptr;
        size_ = 0;
    }

private:
    Node* root_;
    std::size_t size_;

    void copy_tree(Node* node) {
        if (node) {
            insert(node->data);
            copy_tree(node->left);
            copy_tree(node->right);
        }
    }

    void destroy_tree(Node* node) {
        if (node) {
            destroy_tree(node->left);
            destroy_tree(node->right);
            delete node;
        }
    }

    Node* insert_node(Node* node, const T& value) {
        if (!node) return new Node(value);
        if (value < node->data) {
            node->left = insert_node(node->left, value);
        } else if (value > node->data) {
            node->right = insert_node(node->right, value);
        }
        return node;
    }

    Node* insert_node(Node* node, T&& value) {
        if (!node) return new Node(std::move(value));
        if (value < node->data) {
            node->left = insert_node(node->left, std::move(value));
        } else if (value > node->data) {
            node->right = insert_node(node->right, std::move(value));
        }
        return node;
    }

    Node* remove_node(Node* node, const T& value) {
        if (!node) return nullptr;

        if (value < node->data) {
            node->left = remove_node(node->left, value);
        } else if (value > node->data) {
            node->right = remove_node(node->right, value);
        } else {
            if (!node->left) {
                Node* right = node->right;
                delete node;
                return right;
            }
            if (!node->right) {
                Node* left = node->left;
                delete node;
                return left;
            }
            Node* successor = find_min(node->right);
            node->data = successor->data;
            node->right = remove_node(node->right, successor->data);
        }
        return node;
    }

    Node* find_node(Node* node, const T& value) const {
        if (!node) return nullptr;
        if (value < node->data) return find_node(node->left, value);
        if (value > node->data) return find_node(node->right, value);
        return node;
    }

    Node* find_min(Node* node) const {
        while (node->left) node = node->left;
        return node;
    }

    Node* find_max(Node* node) const {
        while (node->right) node = node->right;
        return node;
    }

    void inorder_traverse(Node* node, std::vector<T>& result) const {
        if (node) {
            inorder_traverse(node->left, result);
            result.push_back(node->data);
            inorder_traverse(node->right, result);
        }
    }

    void preorder_traverse(Node* node, std::vector<T>& result) const {
        if (node) {
            result.push_back(node->data);
            preorder_traverse(node->left, result);
            preorder_traverse(node->right, result);
        }
    }

    void postorder_traverse(Node* node, std::vector<T>& result) const {
        if (node) {
            postorder_traverse(node->left, result);
            postorder_traverse(node->right, result);
            result.push_back(node->data);
        }
    }

    std::size_t tree_height(Node* node) const {
        if (!node) return 0;
        std::size_t left_h = tree_height(node->left);
        std::size_t right_h = tree_height(node->right);
        return 1 + (left_h > right_h ? left_h : right_h);
    }
};

} // namespace dsa

#endif // DSA_BST_HPP
