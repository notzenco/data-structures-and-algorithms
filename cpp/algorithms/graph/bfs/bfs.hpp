/**
 * @file bfs.hpp
 * @brief Breadth-First Search algorithm implementation
 *
 * Graph traversal algorithm that explores all neighbors at the current
 * depth before moving to nodes at the next depth level.
 * Time: O(V + E)
 * Space: O(V)
 */

#ifndef DSA_BFS_HPP
#define DSA_BFS_HPP

#include <cstddef>
#include <functional>
#include <optional>
#include <queue>
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
std::vector<T> bfs(const Graph<T>& graph, const T& start) {
    std::vector<T> result;
    if (!graph.has_vertex(start)) {
        return result;
    }

    std::unordered_set<T> visited;
    std::queue<T> queue;

    queue.push(start);
    visited.insert(start);

    while (!queue.empty()) {
        T current = queue.front();
        queue.pop();
        result.push_back(current);

        for (const T& neighbor : graph.neighbors(current)) {
            if (visited.find(neighbor) == visited.end()) {
                visited.insert(neighbor);
                queue.push(neighbor);
            }
        }
    }

    return result;
}

template <typename T, typename Callback>
void bfs(const Graph<T>& graph, const T& start, Callback callback) {
    if (!graph.has_vertex(start)) {
        return;
    }

    std::unordered_set<T> visited;
    std::queue<T> queue;

    queue.push(start);
    visited.insert(start);

    while (!queue.empty()) {
        T current = queue.front();
        queue.pop();
        callback(current);

        for (const T& neighbor : graph.neighbors(current)) {
            if (visited.find(neighbor) == visited.end()) {
                visited.insert(neighbor);
                queue.push(neighbor);
            }
        }
    }
}

template <typename T>
std::optional<std::vector<T>> bfs_path(const Graph<T>& graph, const T& start,
                                        const T& end) {
    if (!graph.has_vertex(start) || !graph.has_vertex(end)) {
        return std::nullopt;
    }

    if (start == end) {
        return std::vector<T>{start};
    }

    std::unordered_set<T> visited;
    std::unordered_map<T, T> parent;
    std::queue<T> queue;

    queue.push(start);
    visited.insert(start);

    while (!queue.empty()) {
        T current = queue.front();
        queue.pop();

        if (current == end) {
            std::vector<T> path;
            T node = end;
            while (node != start) {
                path.push_back(node);
                node = parent[node];
            }
            path.push_back(start);
            std::reverse(path.begin(), path.end());
            return path;
        }

        for (const T& neighbor : graph.neighbors(current)) {
            if (visited.find(neighbor) == visited.end()) {
                visited.insert(neighbor);
                parent[neighbor] = current;
                queue.push(neighbor);
            }
        }
    }

    return std::nullopt;
}

template <typename T>
std::unordered_map<T, std::size_t> bfs_distances(const Graph<T>& graph,
                                                  const T& start) {
    std::unordered_map<T, std::size_t> distances;
    if (!graph.has_vertex(start)) {
        return distances;
    }

    std::unordered_set<T> visited;
    std::queue<std::pair<T, std::size_t>> queue;

    queue.push({start, 0});
    visited.insert(start);
    distances[start] = 0;

    while (!queue.empty()) {
        auto [current, dist] = queue.front();
        queue.pop();

        for (const T& neighbor : graph.neighbors(current)) {
            if (visited.find(neighbor) == visited.end()) {
                visited.insert(neighbor);
                distances[neighbor] = dist + 1;
                queue.push({neighbor, dist + 1});
            }
        }
    }

    return distances;
}

} // namespace dsa

#endif // DSA_BFS_HPP
