<?php

declare(strict_types=1);

namespace DSA\Algorithms;

/**
 * Merge sort algorithm.
 * Time: O(n log n) all cases
 * Space: O(n)
 * Stable: Yes
 */
class MergeSort
{
    /**
     * Sort an array in-place using merge sort.
     *
     * @param array &$arr Array to sort (modified in-place)
     * @param callable|null $comparator Custom comparison function
     */
    public static function sort(array &$arr, ?callable $comparator = null): void
    {
        $comparator = $comparator ?? fn($a, $b) => $a <=> $b;
        $arr = self::mergeSort($arr, $comparator);
    }

    /**
     * Return a sorted copy of the array using merge sort.
     *
     * @param array $arr Array to sort
     * @param callable|null $comparator Custom comparison function
     * @return array Sorted array
     */
    public static function sorted(array $arr, ?callable $comparator = null): array
    {
        $comparator = $comparator ?? fn($a, $b) => $a <=> $b;
        return self::mergeSort($arr, $comparator);
    }

    private static function mergeSort(array $arr, callable $comparator): array
    {
        $n = count($arr);
        if ($n <= 1) {
            return $arr;
        }

        $mid = intdiv($n, 2);
        $left = self::mergeSort(array_slice($arr, 0, $mid), $comparator);
        $right = self::mergeSort(array_slice($arr, $mid), $comparator);

        return self::merge($left, $right, $comparator);
    }

    private static function merge(array $left, array $right, callable $comparator): array
    {
        $result = [];
        $i = 0;
        $j = 0;
        $leftLen = count($left);
        $rightLen = count($right);

        while ($i < $leftLen && $j < $rightLen) {
            if ($comparator($left[$i], $right[$j]) <= 0) {
                $result[] = $left[$i++];
            } else {
                $result[] = $right[$j++];
            }
        }

        while ($i < $leftLen) {
            $result[] = $left[$i++];
        }

        while ($j < $rightLen) {
            $result[] = $right[$j++];
        }

        return $result;
    }
}
