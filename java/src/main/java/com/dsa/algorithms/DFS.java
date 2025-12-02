package com.dsa.algorithms;

import java.util.*;
import java.util.function.Consumer;

/**
 * Depth-First Search implementation.
 * Time: O(V + E)
 * Space: O(V)
 */
public class DFS {

    public static <T> List<T> traverse(Graph<T> graph, T start) {
        return traverse(graph, start, null);
    }

    public static <T> List<T> traverse(Graph<T> graph, T start, Consumer<T> callback) {
        List<T> result = new ArrayList<>();

        if (!graph.hasVertex(start)) {
            return result;
        }

        Set<T> visited = new HashSet<>();
        Deque<T> stack = new ArrayDeque<>();

        stack.push(start);

        while (!stack.isEmpty()) {
            T current = stack.pop();

            if (visited.contains(current)) {
                continue;
            }

            visited.add(current);
            result.add(current);

            if (callback != null) {
                callback.accept(current);
            }

            List<T> neighbors = graph.neighbors(current);
            for (int i = neighbors.size() - 1; i >= 0; i--) {
                T neighbor = neighbors.get(i);
                if (!visited.contains(neighbor)) {
                    stack.push(neighbor);
                }
            }
        }

        return result;
    }

    public static <T> List<T> traverseRecursive(Graph<T> graph, T start) {
        return traverseRecursive(graph, start, null);
    }

    public static <T> List<T> traverseRecursive(Graph<T> graph, T start, Consumer<T> callback) {
        List<T> result = new ArrayList<>();

        if (!graph.hasVertex(start)) {
            return result;
        }

        Set<T> visited = new HashSet<>();
        dfsRecursive(graph, start, visited, result, callback);
        return result;
    }

    private static <T> void dfsRecursive(
            Graph<T> graph, T current, Set<T> visited, List<T> result, Consumer<T> callback) {
        visited.add(current);
        result.add(current);

        if (callback != null) {
            callback.accept(current);
        }

        for (T neighbor : graph.neighbors(current)) {
            if (!visited.contains(neighbor)) {
                dfsRecursive(graph, neighbor, visited, result, callback);
            }
        }
    }

    public static <T> Optional<List<T>> findPath(Graph<T> graph, T start, T end) {
        if (!graph.hasVertex(start) || !graph.hasVertex(end)) {
            return Optional.empty();
        }

        if (start.equals(end)) {
            return Optional.of(Collections.singletonList(start));
        }

        Set<T> visited = new HashSet<>();
        List<T> path = new ArrayList<>();

        if (dfsPath(graph, start, end, visited, path)) {
            return Optional.of(path);
        }
        return Optional.empty();
    }

    private static <T> boolean dfsPath(
            Graph<T> graph, T current, T end, Set<T> visited, List<T> path) {
        visited.add(current);
        path.add(current);

        if (current.equals(end)) {
            return true;
        }

        for (T neighbor : graph.neighbors(current)) {
            if (!visited.contains(neighbor)) {
                if (dfsPath(graph, neighbor, end, visited, path)) {
                    return true;
                }
            }
        }

        path.remove(path.size() - 1);
        return false;
    }

    public static <T> boolean hasCycle(Graph<T> graph, T start) {
        if (!graph.hasVertex(start)) {
            return false;
        }

        Set<T> visited = new HashSet<>();
        Set<T> recStack = new HashSet<>();

        return detectCycle(graph, start, visited, recStack);
    }

    private static <T> boolean detectCycle(
            Graph<T> graph, T current, Set<T> visited, Set<T> recStack) {
        visited.add(current);
        recStack.add(current);

        for (T neighbor : graph.neighbors(current)) {
            if (!visited.contains(neighbor)) {
                if (detectCycle(graph, neighbor, visited, recStack)) {
                    return true;
                }
            } else if (recStack.contains(neighbor)) {
                return true;
            }
        }

        recStack.remove(current);
        return false;
    }
}
