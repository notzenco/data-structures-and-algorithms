<?php

declare(strict_types=1);

namespace DSA\DataStructures;

/**
 * Disjoint Set (Union-Find) implementation with path compression and union by rank.
 * Time: O(α(n)) amortized for all operations (nearly constant)
 * Space: O(n)
 */
class DisjointSet
{
    private array $parent;
    private array $rank;
    private int $count;

    public function __construct(int $size)
    {
        $this->parent = range(0, $size - 1);
        $this->rank = array_fill(0, $size, 0);
        $this->count = $size;
    }

    public function find(int $x): int
    {
        if ($this->parent[$x] !== $x) {
            $this->parent[$x] = $this->find($this->parent[$x]); // Path compression
        }
        return $this->parent[$x];
    }

    public function union(int $x, int $y): bool
    {
        $rootX = $this->find($x);
        $rootY = $this->find($y);

        if ($rootX === $rootY) {
            return false;
        }

        // Union by rank
        if ($this->rank[$rootX] < $this->rank[$rootY]) {
            $this->parent[$rootX] = $rootY;
        } elseif ($this->rank[$rootX] > $this->rank[$rootY]) {
            $this->parent[$rootY] = $rootX;
        } else {
            $this->parent[$rootY] = $rootX;
            $this->rank[$rootX]++;
        }

        $this->count--;
        return true;
    }

    public function connected(int $x, int $y): bool
    {
        return $this->find($x) === $this->find($y);
    }

    public function count(): int
    {
        return $this->count;
    }

    public function size(): int
    {
        return count($this->parent);
    }
}
