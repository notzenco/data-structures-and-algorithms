/**
 * @file dfs.hpp
 * @brief Depth-First Search algorithm implementation
 *
 * Graph traversal algorithm that explores as far as possible along
 * each branch before backtracking.
 * Time: O(V + E)
 * Space: O(V)
 */

#ifndef DSA_DFS_HPP
#define DSA_DFS_HPP

#include <cstddef>
#include <functional>
#include <optional>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace dsa {

template <typename T>
class Graph {
public:
    void add_vertex(const T& vertex) {
        if (adj_list_.find(vertex) == adj_list_.end()) {
            adj_list_[vertex] = {};
        }
    }

    void add_edge(const T& from, const T& to, bool directed = false) {
        add_vertex(from);
        add_vertex(to);
        adj_list_[from].push_back(to);
        if (!directed) {
            adj_list_[to].push_back(from);
        }
    }

    [[nodiscard]] std::vector<T> neighbors(const T& vertex) const {
        auto it = adj_list_.find(vertex);
        if (it != adj_list_.end()) {
            return it->second;
        }
        return {};
    }

    [[nodiscard]] std::vector<T> vertices() const {
        std::vector<T> result;
        for (const auto& [v, _] : adj_list_) {
            result.push_back(v);
        }
        return result;
    }

    [[nodiscard]] bool has_vertex(const T& vertex) const {
        return adj_list_.find(vertex) != adj_list_.end();
    }

    [[nodiscard]] std::size_t vertex_count() const {
        return adj_list_.size();
    }

private:
    std::unordered_map<T, std::vector<T>> adj_list_;
};

template <typename T>
std::vector<T> dfs(const Graph<T>& graph, const T& start) {
    std::vector<T> result;
    if (!graph.has_vertex(start)) {
        return result;
    }

    std::unordered_set<T> visited;
    std::stack<T> stack;

    stack.push(start);

    while (!stack.empty()) {
        T current = stack.top();
        stack.pop();

        if (visited.find(current) != visited.end()) {
            continue;
        }

        visited.insert(current);
        result.push_back(current);

        auto neighbors = graph.neighbors(current);
        for (auto it = neighbors.rbegin(); it != neighbors.rend(); ++it) {
            if (visited.find(*it) == visited.end()) {
                stack.push(*it);
            }
        }
    }

    return result;
}

template <typename T>
void dfs(const Graph<T>& graph, const T& start,
         std::function<void(const T&)> callback) {
    if (!graph.has_vertex(start)) {
        return;
    }

    std::unordered_set<T> visited;
    std::stack<T> stack;

    stack.push(start);

    while (!stack.empty()) {
        T current = stack.top();
        stack.pop();

        if (visited.find(current) != visited.end()) {
            continue;
        }

        visited.insert(current);
        callback(current);

        auto neighbors = graph.neighbors(current);
        for (auto it = neighbors.rbegin(); it != neighbors.rend(); ++it) {
            if (visited.find(*it) == visited.end()) {
                stack.push(*it);
            }
        }
    }
}

namespace detail {

template <typename T>
void dfs_recursive_impl(const Graph<T>& graph, const T& current,
                        std::unordered_set<T>& visited, std::vector<T>& result) {
    visited.insert(current);
    result.push_back(current);

    for (const T& neighbor : graph.neighbors(current)) {
        if (visited.find(neighbor) == visited.end()) {
            dfs_recursive_impl(graph, neighbor, visited, result);
        }
    }
}

template <typename T>
bool dfs_path_impl(const Graph<T>& graph, const T& current, const T& end,
                   std::unordered_set<T>& visited, std::vector<T>& path) {
    visited.insert(current);
    path.push_back(current);

    if (current == end) {
        return true;
    }

    for (const T& neighbor : graph.neighbors(current)) {
        if (visited.find(neighbor) == visited.end()) {
            if (dfs_path_impl(graph, neighbor, end, visited, path)) {
                return true;
            }
        }
    }

    path.pop_back();
    return false;
}

} // namespace detail

template <typename T>
std::vector<T> dfs_recursive(const Graph<T>& graph, const T& start) {
    std::vector<T> result;
    if (!graph.has_vertex(start)) {
        return result;
    }

    std::unordered_set<T> visited;
    detail::dfs_recursive_impl(graph, start, visited, result);
    return result;
}

template <typename T>
std::optional<std::vector<T>> dfs_path(const Graph<T>& graph, const T& start,
                                        const T& end) {
    if (!graph.has_vertex(start) || !graph.has_vertex(end)) {
        return std::nullopt;
    }

    if (start == end) {
        return std::vector<T>{start};
    }

    std::unordered_set<T> visited;
    std::vector<T> path;

    if (detail::dfs_path_impl(graph, start, end, visited, path)) {
        return path;
    }

    return std::nullopt;
}

template <typename T>
bool has_cycle(const Graph<T>& graph, const T& start) {
    if (!graph.has_vertex(start)) {
        return false;
    }

    std::unordered_set<T> visited;
    std::unordered_set<T> rec_stack;

    std::function<bool(const T&)> detect = [&](const T& v) -> bool {
        visited.insert(v);
        rec_stack.insert(v);

        for (const T& neighbor : graph.neighbors(v)) {
            if (visited.find(neighbor) == visited.end()) {
                if (detect(neighbor)) {
                    return true;
                }
            } else if (rec_stack.find(neighbor) != rec_stack.end()) {
                return true;
            }
        }

        rec_stack.erase(v);
        return false;
    };

    return detect(start);
}

} // namespace dsa

#endif // DSA_DFS_HPP
