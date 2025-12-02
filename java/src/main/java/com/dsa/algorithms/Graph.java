package com.dsa.algorithms;

import java.util.*;

/**
 * A generic graph implementation using adjacency list.
 */
public class Graph<T> {
    private final Map<T, List<T>> adjList;
    private final boolean directed;

    public Graph() {
        this(false);
    }

    public Graph(boolean directed) {
        this.adjList = new HashMap<>();
        this.directed = directed;
    }

    public void addVertex(T vertex) {
        adjList.putIfAbsent(vertex, new ArrayList<>());
    }

    public void addEdge(T from, T to) {
        addVertex(from);
        addVertex(to);
        adjList.get(from).add(to);
        if (!directed) {
            adjList.get(to).add(from);
        }
    }

    public List<T> neighbors(T vertex) {
        return adjList.getOrDefault(vertex, Collections.emptyList());
    }

    public boolean hasVertex(T vertex) {
        return adjList.containsKey(vertex);
    }

    public Set<T> vertices() {
        return adjList.keySet();
    }

    public int size() {
        return adjList.size();
    }
}
