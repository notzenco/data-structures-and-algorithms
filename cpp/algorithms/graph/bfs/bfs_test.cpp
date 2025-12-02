/**
 * @file bfs_test.cpp
 * @brief Unit tests for BFS implementation
 */

#include "bfs.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <unordered_set>

void test_empty_graph() {
    std::cout << "  test_empty_graph...";
    dsa::Graph<int> graph;
    auto result = dsa::bfs(graph, 0);
    assert(result.empty());
    std::cout << " PASSED\n";
}

void test_single_vertex() {
    std::cout << "  test_single_vertex...";
    dsa::Graph<int> graph;
    graph.add_vertex(1);
    auto result = dsa::bfs(graph, 1);
    assert(result.size() == 1);
    assert(result[0] == 1);
    std::cout << " PASSED\n";
}

void test_linear_graph() {
    std::cout << "  test_linear_graph...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2);
    graph.add_edge(2, 3);
    graph.add_edge(3, 4);

    auto result = dsa::bfs(graph, 1);
    assert(result.size() == 4);
    assert(result[0] == 1);
    assert(result[1] == 2);
    assert(result[2] == 3);
    assert(result[3] == 4);
    std::cout << " PASSED\n";
}

void test_branching_graph() {
    std::cout << "  test_branching_graph...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2);
    graph.add_edge(1, 3);
    graph.add_edge(2, 4);
    graph.add_edge(3, 5);

    auto result = dsa::bfs(graph, 1);
    assert(result.size() == 5);
    assert(result[0] == 1);

    std::unordered_set<int> level1(result.begin() + 1, result.begin() + 3);
    assert(level1.count(2) && level1.count(3));
    std::cout << " PASSED\n";
}

void test_cycle() {
    std::cout << "  test_cycle...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2);
    graph.add_edge(2, 3);
    graph.add_edge(3, 1);

    auto result = dsa::bfs(graph, 1);
    assert(result.size() == 3);

    std::unordered_set<int> visited(result.begin(), result.end());
    assert(visited.count(1) && visited.count(2) && visited.count(3));
    std::cout << " PASSED\n";
}

void test_disconnected() {
    std::cout << "  test_disconnected...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2);
    graph.add_vertex(3);

    auto result = dsa::bfs(graph, 1);
    assert(result.size() == 2);

    std::unordered_set<int> visited(result.begin(), result.end());
    assert(visited.count(1) && visited.count(2));
    assert(!visited.count(3));
    std::cout << " PASSED\n";
}

void test_path_exists() {
    std::cout << "  test_path_exists...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2);
    graph.add_edge(2, 3);
    graph.add_edge(3, 4);

    auto path = dsa::bfs_path(graph, 1, 4);
    assert(path.has_value());
    assert(path->size() == 4);
    assert((*path)[0] == 1);
    assert((*path)[3] == 4);
    std::cout << " PASSED\n";
}

void test_path_not_exists() {
    std::cout << "  test_path_not_exists...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2);
    graph.add_vertex(3);

    auto path = dsa::bfs_path(graph, 1, 3);
    assert(!path.has_value());
    std::cout << " PASSED\n";
}

void test_path_same_vertex() {
    std::cout << "  test_path_same_vertex...";
    dsa::Graph<int> graph;
    graph.add_vertex(1);

    auto path = dsa::bfs_path(graph, 1, 1);
    assert(path.has_value());
    assert(path->size() == 1);
    assert((*path)[0] == 1);
    std::cout << " PASSED\n";
}

void test_distances() {
    std::cout << "  test_distances...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2);
    graph.add_edge(1, 3);
    graph.add_edge(2, 4);
    graph.add_edge(3, 4);
    graph.add_edge(4, 5);

    auto distances = dsa::bfs_distances(graph, 1);
    assert(distances[1] == 0);
    assert(distances[2] == 1);
    assert(distances[3] == 1);
    assert(distances[4] == 2);
    assert(distances[5] == 3);
    std::cout << " PASSED\n";
}

void test_callback() {
    std::cout << "  test_callback...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2);
    graph.add_edge(2, 3);

    std::vector<int> visited;
    dsa::bfs(graph, 1, [&visited](const int& v) {
        visited.push_back(v);
    });

    assert(visited.size() == 3);
    assert(visited[0] == 1);
    std::cout << " PASSED\n";
}

void test_string_vertices() {
    std::cout << "  test_string_vertices...";
    dsa::Graph<std::string> graph;
    graph.add_edge("A", "B");
    graph.add_edge("B", "C");

    auto result = dsa::bfs(graph, std::string("A"));
    assert(result.size() == 3);
    assert(result[0] == "A");
    std::cout << " PASSED\n";
}

void test_directed_graph() {
    std::cout << "  test_directed_graph...";
    dsa::Graph<int> graph;
    graph.add_edge(1, 2, true);
    graph.add_edge(2, 3, true);

    auto from_1 = dsa::bfs(graph, 1);
    assert(from_1.size() == 3);

    auto from_3 = dsa::bfs(graph, 3);
    assert(from_3.size() == 1);
    std::cout << " PASSED\n";
}

int main() {
    std::cout << "Running BFS tests...\n";
    test_empty_graph();
    test_single_vertex();
    test_linear_graph();
    test_branching_graph();
    test_cycle();
    test_disconnected();
    test_path_exists();
    test_path_not_exists();
    test_path_same_vertex();
    test_distances();
    test_callback();
    test_string_vertices();
    test_directed_graph();
    std::cout << "All BFS tests PASSED!\n";
    return 0;
}
