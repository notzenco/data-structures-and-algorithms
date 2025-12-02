<?php

declare(strict_types=1);

namespace DSA\Algorithms;

/**
 * Graph representation using adjacency list.
 *
 * @template T
 */
class Graph
{
    /** @var array<int|string, array<int|string>> */
    private array $adjacencyList = [];
    private bool $directed;

    public function __construct(bool $directed = false)
    {
        $this->directed = $directed;
    }

    /**
     * Add a vertex to the graph.
     *
     * @param int|string $vertex
     */
    public function addVertex(int|string $vertex): void
    {
        if (!isset($this->adjacencyList[$vertex])) {
            $this->adjacencyList[$vertex] = [];
        }
    }

    /**
     * Add an edge between two vertices.
     *
     * @param int|string $from Source vertex
     * @param int|string $to Destination vertex
     */
    public function addEdge(int|string $from, int|string $to): void
    {
        $this->addVertex($from);
        $this->addVertex($to);

        if (!in_array($to, $this->adjacencyList[$from], true)) {
            $this->adjacencyList[$from][] = $to;
        }

        if (!$this->directed && !in_array($from, $this->adjacencyList[$to], true)) {
            $this->adjacencyList[$to][] = $from;
        }
    }

    /**
     * Remove an edge between two vertices.
     *
     * @param int|string $from Source vertex
     * @param int|string $to Destination vertex
     */
    public function removeEdge(int|string $from, int|string $to): void
    {
        if (isset($this->adjacencyList[$from])) {
            $this->adjacencyList[$from] = array_values(
                array_filter($this->adjacencyList[$from], fn($v) => $v !== $to)
            );
        }

        if (!$this->directed && isset($this->adjacencyList[$to])) {
            $this->adjacencyList[$to] = array_values(
                array_filter($this->adjacencyList[$to], fn($v) => $v !== $from)
            );
        }
    }

    /**
     * Remove a vertex and all its edges.
     *
     * @param int|string $vertex
     */
    public function removeVertex(int|string $vertex): void
    {
        if (!isset($this->adjacencyList[$vertex])) {
            return;
        }

        unset($this->adjacencyList[$vertex]);

        foreach ($this->adjacencyList as $v => $neighbors) {
            $this->adjacencyList[$v] = array_values(
                array_filter($neighbors, fn($n) => $n !== $vertex)
            );
        }
    }

    /**
     * Check if two vertices are connected by an edge.
     *
     * @param int|string $from Source vertex
     * @param int|string $to Destination vertex
     * @return bool
     */
    public function hasEdge(int|string $from, int|string $to): bool
    {
        return isset($this->adjacencyList[$from]) && in_array($to, $this->adjacencyList[$from], true);
    }

    /**
     * Get all neighbors of a vertex.
     *
     * @param int|string $vertex
     * @return array<int|string>
     */
    public function getNeighbors(int|string $vertex): array
    {
        return $this->adjacencyList[$vertex] ?? [];
    }

    /**
     * Get all vertices in the graph.
     *
     * @return array<int|string>
     */
    public function getVertices(): array
    {
        return array_keys($this->adjacencyList);
    }

    /**
     * Get the number of vertices.
     *
     * @return int
     */
    public function vertexCount(): int
    {
        return count($this->adjacencyList);
    }

    /**
     * Check if the graph is directed.
     *
     * @return bool
     */
    public function isDirected(): bool
    {
        return $this->directed;
    }

    /**
     * Get the adjacency list representation.
     *
     * @return array<int|string, array<int|string>>
     */
    public function getAdjacencyList(): array
    {
        return $this->adjacencyList;
    }
}
