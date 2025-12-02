<?php

declare(strict_types=1);

namespace DSA\Algorithms;

/**
 * Quick sort algorithm with median-of-three pivot selection.
 * Time: O(n log n) average, O(n^2) worst case
 * Space: O(log n) average for recursion stack
 * Stable: No
 */
class QuickSort
{
    /**
     * Sort an array in-place using quick sort.
     *
     * @param array &$arr Array to sort (modified in-place)
     * @param callable|null $comparator Custom comparison function
     */
    public static function sort(array &$arr, ?callable $comparator = null): void
    {
        $comparator = $comparator ?? fn($a, $b) => $a <=> $b;
        self::quickSort($arr, 0, count($arr) - 1, $comparator);
    }

    /**
     * Return a sorted copy of the array using quick sort.
     *
     * @param array $arr Array to sort
     * @param callable|null $comparator Custom comparison function
     * @return array Sorted array
     */
    public static function sorted(array $arr, ?callable $comparator = null): array
    {
        self::sort($arr, $comparator);
        return $arr;
    }

    private static function quickSort(array &$arr, int $low, int $high, callable $comparator): void
    {
        if ($low < $high) {
            $pivotIndex = self::partition($arr, $low, $high, $comparator);
            self::quickSort($arr, $low, $pivotIndex - 1, $comparator);
            self::quickSort($arr, $pivotIndex + 1, $high, $comparator);
        }
    }

    private static function partition(array &$arr, int $low, int $high, callable $comparator): int
    {
        // Median-of-three pivot selection
        $mid = $low + intdiv($high - $low, 2);

        if ($comparator($arr[$mid], $arr[$low]) < 0) {
            self::swap($arr, $low, $mid);
        }
        if ($comparator($arr[$high], $arr[$low]) < 0) {
            self::swap($arr, $low, $high);
        }
        if ($comparator($arr[$mid], $arr[$high]) < 0) {
            self::swap($arr, $mid, $high);
        }

        $pivot = $arr[$high];
        $i = $low - 1;

        for ($j = $low; $j < $high; $j++) {
            if ($comparator($arr[$j], $pivot) <= 0) {
                $i++;
                self::swap($arr, $i, $j);
            }
        }

        self::swap($arr, $i + 1, $high);
        return $i + 1;
    }

    private static function swap(array &$arr, int $i, int $j): void
    {
        [$arr[$i], $arr[$j]] = [$arr[$j], $arr[$i]];
    }
}
