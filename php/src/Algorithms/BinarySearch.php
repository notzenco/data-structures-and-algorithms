<?php

declare(strict_types=1);

namespace DSA\Algorithms;

/**
 * Binary search algorithms.
 * Time: O(log n)
 * Space: O(1) iterative, O(log n) recursive
 */
class BinarySearch
{
    /**
     * Find the index of target in a sorted array.
     * Returns -1 if not found.
     *
     * @param array $arr Sorted array
     * @param mixed $target Value to find
     * @param callable|null $comparator Custom comparison function
     * @return int Index of target or -1
     */
    public static function search(array $arr, mixed $target, ?callable $comparator = null): int
    {
        $comparator = $comparator ?? fn($a, $b) => $a <=> $b;
        $left = 0;
        $right = count($arr) - 1;

        while ($left <= $right) {
            $mid = $left + intdiv($right - $left, 2);
            $cmp = $comparator($arr[$mid], $target);

            if ($cmp === 0) {
                return $mid;
            } elseif ($cmp < 0) {
                $left = $mid + 1;
            } else {
                $right = $mid - 1;
            }
        }

        return -1;
    }

    /**
     * Find the leftmost (first) index where target could be inserted to maintain sorted order.
     * Returns the index of the first element >= target.
     *
     * @param array $arr Sorted array
     * @param mixed $target Value to find
     * @param callable|null $comparator Custom comparison function
     * @return int Insertion index
     */
    public static function lowerBound(array $arr, mixed $target, ?callable $comparator = null): int
    {
        $comparator = $comparator ?? fn($a, $b) => $a <=> $b;
        $left = 0;
        $right = count($arr);

        while ($left < $right) {
            $mid = $left + intdiv($right - $left, 2);
            if ($comparator($arr[$mid], $target) < 0) {
                $left = $mid + 1;
            } else {
                $right = $mid;
            }
        }

        return $left;
    }

    /**
     * Find the rightmost index where target could be inserted to maintain sorted order.
     * Returns the index of the first element > target.
     *
     * @param array $arr Sorted array
     * @param mixed $target Value to find
     * @param callable|null $comparator Custom comparison function
     * @return int Insertion index
     */
    public static function upperBound(array $arr, mixed $target, ?callable $comparator = null): int
    {
        $comparator = $comparator ?? fn($a, $b) => $a <=> $b;
        $left = 0;
        $right = count($arr);

        while ($left < $right) {
            $mid = $left + intdiv($right - $left, 2);
            if ($comparator($arr[$mid], $target) <= 0) {
                $left = $mid + 1;
            } else {
                $right = $mid;
            }
        }

        return $left;
    }
}
