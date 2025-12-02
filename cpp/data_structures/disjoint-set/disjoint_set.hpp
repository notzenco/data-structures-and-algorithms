/**
 * @file disjoint_set.hpp
 * @brief Disjoint Set (Union-Find) data structure implementation
 *
 * Tracks elements partitioned into disjoint sets.
 * Uses union by rank and path compression for near O(1) operations.
 */

#ifndef DSA_DISJOINT_SET_HPP
#define DSA_DISJOINT_SET_HPP

#include <cstddef>
#include <vector>

namespace dsa {

class DisjointSet {
public:
    explicit DisjointSet(std::size_t n)
        : parent_(n), rank_(n, 0), size_(n, 1), num_sets_(n) {
        for (std::size_t i = 0; i < n; ++i) {
            parent_[i] = i;
        }
    }

    DisjointSet(const DisjointSet&) = default;
    DisjointSet(DisjointSet&&) noexcept = default;
    DisjointSet& operator=(const DisjointSet&) = default;
    DisjointSet& operator=(DisjointSet&&) noexcept = default;
    ~DisjointSet() = default;

    std::size_t find(std::size_t x) {
        if (x >= parent_.size()) return x;
        if (parent_[x] != x) {
            parent_[x] = find(parent_[x]);
        }
        return parent_[x];
    }

    bool unite(std::size_t x, std::size_t y) {
        std::size_t root_x = find(x);
        std::size_t root_y = find(y);

        if (root_x == root_y) return false;

        if (rank_[root_x] < rank_[root_y]) {
            parent_[root_x] = root_y;
            size_[root_y] += size_[root_x];
        } else if (rank_[root_x] > rank_[root_y]) {
            parent_[root_y] = root_x;
            size_[root_x] += size_[root_y];
        } else {
            parent_[root_y] = root_x;
            size_[root_x] += size_[root_y];
            ++rank_[root_x];
        }

        --num_sets_;
        return true;
    }

    [[nodiscard]] bool connected(std::size_t x, std::size_t y) {
        return find(x) == find(y);
    }

    [[nodiscard]] std::size_t set_size(std::size_t x) {
        return size_[find(x)];
    }

    [[nodiscard]] std::size_t num_sets() const noexcept {
        return num_sets_;
    }

    [[nodiscard]] std::size_t size() const noexcept {
        return parent_.size();
    }

private:
    std::vector<std::size_t> parent_;
    std::vector<std::size_t> rank_;
    std::vector<std::size_t> size_;
    std::size_t num_sets_;
};

} // namespace dsa

#endif // DSA_DISJOINT_SET_HPP
