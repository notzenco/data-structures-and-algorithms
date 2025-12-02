<?php

declare(strict_types=1);

namespace DSA\Algorithms;

/**
 * Insertion sort algorithm.
 * Time: O(n^2) worst/average, O(n) best (already sorted)
 * Space: O(1)
 * Stable: Yes
 */
class InsertionSort
{
    /**
     * Sort an array in-place using insertion sort.
     *
     * @param array &$arr Array to sort (modified in-place)
     * @param callable|null $comparator Custom comparison function
     */
    public static function sort(array &$arr, ?callable $comparator = null): void
    {
        $comparator = $comparator ?? fn($a, $b) => $a <=> $b;
        $n = count($arr);

        for ($i = 1; $i < $n; $i++) {
            $key = $arr[$i];
            $j = $i - 1;

            while ($j >= 0 && $comparator($arr[$j], $key) > 0) {
                $arr[$j + 1] = $arr[$j];
                $j--;
            }
            $arr[$j + 1] = $key;
        }
    }

    /**
     * Return a sorted copy of the array using insertion sort.
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
}
