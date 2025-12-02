package com.dsa.datastructures;

/**
 * A disjoint set (union-find) implementation with path compression and union by rank.
 * Time: O(α(n)) amortized for find/union (nearly constant)
 * Space: O(n)
 */
public class DisjointSet {
    private final int[] parent;
    private final int[] rank;
    private final int[] size;
    private int numSets;

    public DisjointSet(int n) {
        parent = new int[n];
        rank = new int[n];
        size = new int[n];
        numSets = n;

        for (int i = 0; i < n; i++) {
            parent[i] = i;
            rank[i] = 0;
            size[i] = 1;
        }
    }

    public int find(int x) {
        if (parent[x] != x) {
            parent[x] = find(parent[x]); // Path compression
        }
        return parent[x];
    }

    public boolean union(int x, int y) {
        int rootX = find(x);
        int rootY = find(y);

        if (rootX == rootY) {
            return false;
        }

        // Union by rank
        if (rank[rootX] < rank[rootY]) {
            parent[rootX] = rootY;
            size[rootY] += size[rootX];
        } else if (rank[rootX] > rank[rootY]) {
            parent[rootY] = rootX;
            size[rootX] += size[rootY];
        } else {
            parent[rootY] = rootX;
            size[rootX] += size[rootY];
            rank[rootX]++;
        }

        numSets--;
        return true;
    }

    public boolean connected(int x, int y) {
        return find(x) == find(y);
    }

    public int setSize(int x) {
        return size[find(x)];
    }

    public int numSets() {
        return numSets;
    }

    public int size() {
        return parent.length;
    }
}
