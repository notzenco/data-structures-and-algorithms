<?php

declare(strict_types=1);

namespace DSA\Tests;

use PHPUnit\Framework\TestCase;
use DSA\Algorithms\BinarySearch;
use DSA\Algorithms\InsertionSort;
use DSA\Algorithms\MergeSort;
use DSA\Algorithms\QuickSort;
use DSA\Algorithms\Graph;
use DSA\Algorithms\BFS;
use DSA\Algorithms\DFS;

class AlgorithmsTest extends TestCase
{
    // Binary Search Tests
    public function testBinarySearch(): void
    {
        $arr = [1, 3, 5, 7, 9, 11, 13];

        $this->assertEquals(0, BinarySearch::search($arr, 1));
        $this->assertEquals(3, BinarySearch::search($arr, 7));
        $this->assertEquals(6, BinarySearch::search($arr, 13));
        $this->assertEquals(-1, BinarySearch::search($arr, 4));
        $this->assertEquals(-1, BinarySearch::search($arr, 0));
        $this->assertEquals(-1, BinarySearch::search($arr, 14));
    }

    public function testBinarySearchLowerBound(): void
    {
        $arr = [1, 3, 5, 7, 9];

        $this->assertEquals(0, BinarySearch::lowerBound($arr, 1));
        $this->assertEquals(2, BinarySearch::lowerBound($arr, 5));
        $this->assertEquals(2, BinarySearch::lowerBound($arr, 4)); // Between 3 and 5
        $this->assertEquals(0, BinarySearch::lowerBound($arr, 0)); // Before all
        $this->assertEquals(5, BinarySearch::lowerBound($arr, 10)); // After all
    }

    public function testBinarySearchUpperBound(): void
    {
        $arr = [1, 3, 5, 7, 9];

        $this->assertEquals(1, BinarySearch::upperBound($arr, 1));
        $this->assertEquals(3, BinarySearch::upperBound($arr, 5));
        $this->assertEquals(2, BinarySearch::upperBound($arr, 4)); // Between 3 and 5
        $this->assertEquals(0, BinarySearch::upperBound($arr, 0)); // Before all
        $this->assertEquals(5, BinarySearch::upperBound($arr, 10)); // After all
    }

    // Insertion Sort Tests
    public function testInsertionSort(): void
    {
        $arr = [64, 34, 25, 12, 22, 11, 90];
        InsertionSort::sort($arr);
        $this->assertEquals([11, 12, 22, 25, 34, 64, 90], $arr);
    }

    public function testInsertionSortSorted(): void
    {
        $arr = [5, 2, 8, 1, 9];
        $sorted = InsertionSort::sorted($arr);
        $this->assertEquals([1, 2, 5, 8, 9], $sorted);
        $this->assertEquals([5, 2, 8, 1, 9], $arr); // Original unchanged
    }

    public function testInsertionSortDescending(): void
    {
        $arr = [3, 1, 4, 1, 5];
        InsertionSort::sort($arr, fn($a, $b) => $b <=> $a);
        $this->assertEquals([5, 4, 3, 1, 1], $arr);
    }

    // Merge Sort Tests
    public function testMergeSort(): void
    {
        $arr = [64, 34, 25, 12, 22, 11, 90];
        MergeSort::sort($arr);
        $this->assertEquals([11, 12, 22, 25, 34, 64, 90], $arr);
    }

    public function testMergeSortSorted(): void
    {
        $arr = [5, 2, 8, 1, 9];
        $sorted = MergeSort::sorted($arr);
        $this->assertEquals([1, 2, 5, 8, 9], $sorted);
    }

    public function testMergeSortEmpty(): void
    {
        $arr = [];
        MergeSort::sort($arr);
        $this->assertEquals([], $arr);
    }

    public function testMergeSortSingleElement(): void
    {
        $arr = [42];
        MergeSort::sort($arr);
        $this->assertEquals([42], $arr);
    }

    // Quick Sort Tests
    public function testQuickSort(): void
    {
        $arr = [64, 34, 25, 12, 22, 11, 90];
        QuickSort::sort($arr);
        $this->assertEquals([11, 12, 22, 25, 34, 64, 90], $arr);
    }

    public function testQuickSortSorted(): void
    {
        $arr = [5, 2, 8, 1, 9];
        $sorted = QuickSort::sorted($arr);
        $this->assertEquals([1, 2, 5, 8, 9], $sorted);
    }

    public function testQuickSortWithDuplicates(): void
    {
        $arr = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3];
        QuickSort::sort($arr);
        $this->assertEquals([1, 1, 2, 3, 3, 4, 5, 5, 6, 9], $arr);
    }

    // Graph Tests
    public function testGraphOperations(): void
    {
        $graph = new Graph();

        $graph->addEdge(1, 2);
        $graph->addEdge(1, 3);
        $graph->addEdge(2, 4);

        $this->assertTrue($graph->hasEdge(1, 2));
        $this->assertTrue($graph->hasEdge(2, 1)); // Undirected
        $this->assertFalse($graph->hasEdge(1, 4));

        $this->assertEquals([2, 3], $graph->getNeighbors(1));

        $graph->removeEdge(1, 2);
        $this->assertFalse($graph->hasEdge(1, 2));
    }

    public function testDirectedGraph(): void
    {
        $graph = new Graph(true);

        $graph->addEdge(1, 2);
        $graph->addEdge(2, 3);

        $this->assertTrue($graph->hasEdge(1, 2));
        $this->assertFalse($graph->hasEdge(2, 1)); // Directed
        $this->assertTrue($graph->isDirected());
    }

    // BFS Tests
    public function testBFSTraverse(): void
    {
        $graph = new Graph();
        $graph->addEdge(1, 2);
        $graph->addEdge(1, 3);
        $graph->addEdge(2, 4);
        $graph->addEdge(3, 4);

        $result = BFS::traverse($graph, 1);
        $this->assertEquals(1, $result[0]);
        $this->assertCount(4, $result);
        $this->assertContains(2, $result);
        $this->assertContains(3, $result);
        $this->assertContains(4, $result);
    }

    public function testBFSShortestPath(): void
    {
        $graph = new Graph();
        $graph->addEdge(1, 2);
        $graph->addEdge(1, 3);
        $graph->addEdge(2, 4);
        $graph->addEdge(3, 4);
        $graph->addEdge(4, 5);

        $path = BFS::shortestPath($graph, 1, 5);
        $this->assertNotNull($path);
        $this->assertEquals(1, $path[0]);
        $this->assertEquals(5, $path[count($path) - 1]);
        $this->assertCount(4, $path); // 1 -> 2 or 3 -> 4 -> 5
    }

    public function testBFSDistances(): void
    {
        $graph = new Graph();
        $graph->addEdge(1, 2);
        $graph->addEdge(1, 3);
        $graph->addEdge(2, 4);
        $graph->addEdge(3, 4);
        $graph->addEdge(4, 5);

        $distances = BFS::distances($graph, 1);
        $this->assertEquals(0, $distances[1]);
        $this->assertEquals(1, $distances[2]);
        $this->assertEquals(1, $distances[3]);
        $this->assertEquals(2, $distances[4]);
        $this->assertEquals(3, $distances[5]);
    }

    // DFS Tests
    public function testDFSTraverse(): void
    {
        $graph = new Graph();
        $graph->addEdge(1, 2);
        $graph->addEdge(1, 3);
        $graph->addEdge(2, 4);
        $graph->addEdge(3, 4);

        $result = DFS::traverse($graph, 1);
        $this->assertEquals(1, $result[0]);
        $this->assertCount(4, $result);
    }

    public function testDFSTraverseRecursive(): void
    {
        $graph = new Graph();
        $graph->addEdge(1, 2);
        $graph->addEdge(1, 3);
        $graph->addEdge(2, 4);

        $result = DFS::traverseRecursive($graph, 1);
        $this->assertEquals(1, $result[0]);
        $this->assertCount(4, $result);
    }

    public function testDFSFindPath(): void
    {
        $graph = new Graph();
        $graph->addEdge(1, 2);
        $graph->addEdge(2, 3);
        $graph->addEdge(3, 4);

        $path = DFS::findPath($graph, 1, 4);
        $this->assertNotNull($path);
        $this->assertEquals(1, $path[0]);
        $this->assertEquals(4, $path[count($path) - 1]);
    }

    public function testDFSHasCycle(): void
    {
        $graphWithCycle = new Graph(true);
        $graphWithCycle->addEdge(1, 2);
        $graphWithCycle->addEdge(2, 3);
        $graphWithCycle->addEdge(3, 1);

        $this->assertTrue(DFS::hasCycle($graphWithCycle));

        $graphWithoutCycle = new Graph(true);
        $graphWithoutCycle->addEdge(1, 2);
        $graphWithoutCycle->addEdge(2, 3);

        $this->assertFalse(DFS::hasCycle($graphWithoutCycle));
    }

    public function testDFSTopologicalSort(): void
    {
        $graph = new Graph(true);
        $graph->addEdge(5, 2);
        $graph->addEdge(5, 0);
        $graph->addEdge(4, 0);
        $graph->addEdge(4, 1);
        $graph->addEdge(2, 3);
        $graph->addEdge(3, 1);

        $sorted = DFS::topologicalSort($graph);
        $this->assertNotNull($sorted);

        // Verify ordering: 5 before 2, 2 before 3, 3 before 1, etc.
        $positions = array_flip($sorted);
        $this->assertLessThan($positions[2], $positions[5]);
        $this->assertLessThan($positions[3], $positions[2]);
        $this->assertLessThan($positions[1], $positions[3]);
    }
}
