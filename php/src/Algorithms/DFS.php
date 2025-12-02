<?php

declare(strict_types=1);

namespace DSA\Algorithms;

use DSA\DataStructures\Stack;

/**
 * Depth-First Search algorithm.
 * Time: O(V + E) where V = vertices, E = edges
 * Space: O(V)
 */
class DFS
{
    /**
     * Perform iterative DFS traversal from a starting vertex.
     *
     * @param Graph $graph The graph to traverse
     * @param int|string $start Starting vertex
     * @return array<int|string> Vertices in DFS order
     */
    public static function traverse(Graph $graph, int|string $start): array
    {
        $result = [];
        $visited = [];

        if (!in_array($start, $graph->getVertices(), true)) {
            return $result;
        }

        $stack = new Stack();
        $stack->push($start);

        while (!$stack->isEmpty()) {
            $vertex = $stack->pop();

            if (isset($visited[$vertex])) {
                continue;
            }

            $visited[$vertex] = true;
            $result[] = $vertex;

            // Push neighbors in reverse order to visit them in natural order
            $neighbors = $graph->getNeighbors($vertex);
            for ($i = count($neighbors) - 1; $i >= 0; $i--) {
                if (!isset($visited[$neighbors[$i]])) {
                    $stack->push($neighbors[$i]);
                }
            }
        }

        return $result;
    }

    /**
     * Perform recursive DFS traversal from a starting vertex.
     *
     * @param Graph $graph The graph to traverse
     * @param int|string $start Starting vertex
     * @return array<int|string> Vertices in DFS order
     */
    public static function traverseRecursive(Graph $graph, int|string $start): array
    {
        $result = [];
        $visited = [];

        if (!in_array($start, $graph->getVertices(), true)) {
            return $result;
        }

        self::dfsRecursive($graph, $start, $visited, $result);
        return $result;
    }

    /**
     * Find a path between two vertices using DFS.
     *
     * @param Graph $graph The graph to search
     * @param int|string $start Starting vertex
     * @param int|string $end Target vertex
     * @return array<int|string>|null Path from start to end, or null if no path exists
     */
    public static function findPath(Graph $graph, int|string $start, int|string $end): ?array
    {
        if (!in_array($start, $graph->getVertices(), true) ||
            !in_array($end, $graph->getVertices(), true)) {
            return null;
        }

        if ($start === $end) {
            return [$start];
        }

        $visited = [];
        $parent = [$start => null];
        $stack = new Stack();
        $stack->push($start);

        while (!$stack->isEmpty()) {
            $vertex = $stack->pop();

            if (isset($visited[$vertex])) {
                continue;
            }

            $visited[$vertex] = true;

            if ($vertex === $end) {
                return self::reconstructPath($parent, $start, $end);
            }

            foreach ($graph->getNeighbors($vertex) as $neighbor) {
                if (!isset($visited[$neighbor])) {
                    $parent[$neighbor] = $vertex;
                    $stack->push($neighbor);
                }
            }
        }

        return null;
    }

    /**
     * Detect if the graph contains a cycle.
     *
     * @param Graph $graph The graph to check
     * @return bool True if a cycle exists
     */
    public static function hasCycle(Graph $graph): bool
    {
        $visited = [];
        $recursionStack = [];

        foreach ($graph->getVertices() as $vertex) {
            if (!isset($visited[$vertex])) {
                if (self::hasCycleUtil($graph, $vertex, $visited, $recursionStack)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Perform topological sort on a directed acyclic graph.
     *
     * @param Graph $graph The graph to sort (must be directed and acyclic)
     * @return array<int|string>|null Topologically sorted vertices, or null if cycle exists
     */
    public static function topologicalSort(Graph $graph): ?array
    {
        if (!$graph->isDirected()) {
            return null;
        }

        $visited = [];
        $result = [];

        foreach ($graph->getVertices() as $vertex) {
            if (!isset($visited[$vertex])) {
                if (!self::topologicalSortUtil($graph, $vertex, $visited, $result, [])) {
                    return null; // Cycle detected
                }
            }
        }

        return array_reverse($result);
    }

    private static function dfsRecursive(
        Graph $graph,
        int|string $vertex,
        array &$visited,
        array &$result
    ): void {
        $visited[$vertex] = true;
        $result[] = $vertex;

        foreach ($graph->getNeighbors($vertex) as $neighbor) {
            if (!isset($visited[$neighbor])) {
                self::dfsRecursive($graph, $neighbor, $visited, $result);
            }
        }
    }

    private static function hasCycleUtil(
        Graph $graph,
        int|string $vertex,
        array &$visited,
        array &$recursionStack
    ): bool {
        $visited[$vertex] = true;
        $recursionStack[$vertex] = true;

        foreach ($graph->getNeighbors($vertex) as $neighbor) {
            if (!isset($visited[$neighbor])) {
                if (self::hasCycleUtil($graph, $neighbor, $visited, $recursionStack)) {
                    return true;
                }
            } elseif (isset($recursionStack[$neighbor])) {
                return true;
            }
        }

        unset($recursionStack[$vertex]);
        return false;
    }

    private static function topologicalSortUtil(
        Graph $graph,
        int|string $vertex,
        array &$visited,
        array &$result,
        array $recursionStack
    ): bool {
        $visited[$vertex] = true;
        $recursionStack[$vertex] = true;

        foreach ($graph->getNeighbors($vertex) as $neighbor) {
            if (isset($recursionStack[$neighbor])) {
                return false; // Cycle detected
            }
            if (!isset($visited[$neighbor])) {
                if (!self::topologicalSortUtil($graph, $neighbor, $visited, $result, $recursionStack)) {
                    return false;
                }
            }
        }

        $result[] = $vertex;
        return true;
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
