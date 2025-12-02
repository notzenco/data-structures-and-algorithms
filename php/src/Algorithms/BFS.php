<?php

declare(strict_types=1);

namespace DSA\Algorithms;

use DSA\DataStructures\Queue;

/**
 * Breadth-First Search algorithm.
 * Time: O(V + E) where V = vertices, E = edges
 * Space: O(V)
 */
class BFS
{
    /**
     * Perform BFS traversal from a starting vertex.
     *
     * @param Graph $graph The graph to traverse
     * @param int|string $start Starting vertex
     * @return array<int|string> Vertices in BFS order
     */
    public static function traverse(Graph $graph, int|string $start): array
    {
        $result = [];
        $visited = [];

        if (!in_array($start, $graph->getVertices(), true)) {
            return $result;
        }

        $queue = new Queue();
        $queue->enqueue($start);
        $visited[$start] = true;

        while (!$queue->isEmpty()) {
            $vertex = $queue->dequeue();
            $result[] = $vertex;

            foreach ($graph->getNeighbors($vertex) as $neighbor) {
                if (!isset($visited[$neighbor])) {
                    $visited[$neighbor] = true;
                    $queue->enqueue($neighbor);
                }
            }
        }

        return $result;
    }

    /**
     * Find the shortest path between two vertices using BFS.
     * Only works for unweighted graphs.
     *
     * @param Graph $graph The graph to search
     * @param int|string $start Starting vertex
     * @param int|string $end Target vertex
     * @return array<int|string>|null Path from start to end, or null if no path exists
     */
    public static function shortestPath(Graph $graph, int|string $start, int|string $end): ?array
    {
        if (!in_array($start, $graph->getVertices(), true) ||
            !in_array($end, $graph->getVertices(), true)) {
            return null;
        }

        if ($start === $end) {
            return [$start];
        }

        $queue = new Queue();
        $queue->enqueue($start);
        $visited = [$start => true];
        $parent = [$start => null];

        while (!$queue->isEmpty()) {
            $vertex = $queue->dequeue();

            foreach ($graph->getNeighbors($vertex) as $neighbor) {
                if (!isset($visited[$neighbor])) {
                    $visited[$neighbor] = true;
                    $parent[$neighbor] = $vertex;

                    if ($neighbor === $end) {
                        return self::reconstructPath($parent, $start, $end);
                    }

                    $queue->enqueue($neighbor);
                }
            }
        }

        return null;
    }

    /**
     * Find distances from start vertex to all reachable vertices.
     *
     * @param Graph $graph The graph to search
     * @param int|string $start Starting vertex
     * @return array<int|string, int> Map of vertex to distance from start
     */
    public static function distances(Graph $graph, int|string $start): array
    {
        $distances = [];

        if (!in_array($start, $graph->getVertices(), true)) {
            return $distances;
        }

        $queue = new Queue();
        $queue->enqueue($start);
        $distances[$start] = 0;

        while (!$queue->isEmpty()) {
            $vertex = $queue->dequeue();
            $currentDistance = $distances[$vertex];

            foreach ($graph->getNeighbors($vertex) as $neighbor) {
                if (!isset($distances[$neighbor])) {
                    $distances[$neighbor] = $currentDistance + 1;
                    $queue->enqueue($neighbor);
                }
            }
        }

        return $distances;
    }

    /**
     * @param array<int|string, int|string|null> $parent
     * @param int|string $start
     * @param int|string $end
     * @return array<int|string>
     */
    private static function reconstructPath(array $parent, int|string $start, int|string $end): array
    {
        $path = [];
        $current = $end;

        while ($current !== null) {
            array_unshift($path, $current);
            $current = $parent[$current];
        }

        return $path;
    }
}
