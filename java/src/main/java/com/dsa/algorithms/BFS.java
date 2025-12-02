package com.dsa.algorithms;

import java.util.*;
import java.util.function.Consumer;

/**
 * Breadth-First Search implementation.
 * Time: O(V + E)
 * Space: O(V)
 */
public class BFS {

    public static <T> List<T> traverse(Graph<T> graph, T start) {
        return traverse(graph, start, null);
    }

    public static <T> List<T> traverse(Graph<T> graph, T start, Consumer<T> callback) {
        List<T> result = new ArrayList<>();

        if (!graph.hasVertex(start)) {
            return result;
        }

        Set<T> visited = new HashSet<>();
        java.util.Queue<T> queue = new LinkedList<>();

        queue.add(start);
        visited.add(start);

        while (!queue.isEmpty()) {
            T current = queue.poll();
            result.add(current);

            if (callback != null) {
                callback.accept(current);
            }

            for (T neighbor : graph.neighbors(current)) {
                if (!visited.contains(neighbor)) {
                    visited.add(neighbor);
                    queue.add(neighbor);
                }
            }
        }

        return result;
    }

    public static <T> Optional<List<T>> findPath(Graph<T> graph, T start, T end) {
        if (!graph.hasVertex(start) || !graph.hasVertex(end)) {
            return Optional.empty();
        }

        if (start.equals(end)) {
            return Optional.of(Collections.singletonList(start));
        }

        Set<T> visited = new HashSet<>();
        Map<T, T> parent = new HashMap<>();
        java.util.Queue<T> queue = new LinkedList<>();

        queue.add(start);
        visited.add(start);

        while (!queue.isEmpty()) {
            T current = queue.poll();

            if (current.equals(end)) {
                List<T> path = new ArrayList<>();
                T node = end;
                while (!node.equals(start)) {
                    path.add(node);
                    node = parent.get(node);
                }
                path.add(start);
                Collections.reverse(path);
                return Optional.of(path);
            }

            for (T neighbor : graph.neighbors(current)) {
                if (!visited.contains(neighbor)) {
                    visited.add(neighbor);
                    parent.put(neighbor, current);
                    queue.add(neighbor);
                }
            }
        }

        return Optional.empty();
    }

    public static <T> Map<T, Integer> distances(Graph<T> graph, T start) {
        Map<T, Integer> distances = new HashMap<>();

        if (!graph.hasVertex(start)) {
            return distances;
        }

        Set<T> visited = new HashSet<>();
        java.util.Queue<Map.Entry<T, Integer>> queue = new LinkedList<>();

        queue.add(Map.entry(start, 0));
        visited.add(start);
        distances.put(start, 0);

        while (!queue.isEmpty()) {
            Map.Entry<T, Integer> entry = queue.poll();
            T current = entry.getKey();
            int dist = entry.getValue();

            for (T neighbor : graph.neighbors(current)) {
                if (!visited.contains(neighbor)) {
                    visited.add(neighbor);
                    distances.put(neighbor, dist + 1);
                    queue.add(Map.entry(neighbor, dist + 1));
                }
            }
        }

        return distances;
    }
}
