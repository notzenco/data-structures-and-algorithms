{-|
Module      : Algorithms.QuickSort
Description : Quick sort algorithm
Stability   : stable

A purely functional quick sort implementation with median-of-three pivot.
Time: O(n log n) average, O(n²) worst case
Space: O(n) - creates new lists
-}
module Algorithms.QuickSort
  ( sort
  , sortBy
  ) where

-- | Sort a list using quick sort
sort :: Ord a => [a] -> [a]
sort = sortBy compare

-- | Sort with custom comparison function
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [x] = [x]
sortBy cmp xs = sortBy cmp lesser ++ [pivot] ++ sortBy cmp greater
  where
    pivot = medianOfThree cmp xs
    lesser  = filter (\x -> cmp x pivot == LT) (filter (/= pivot) xs)
    greater = filter (\x -> cmp x pivot /= LT) (filter (/= pivot) xs)

-- | Select median of first, middle, and last elements
medianOfThree :: (a -> a -> Ordering) -> [a] -> a
medianOfThree _ [x] = x
medianOfThree _ [x, _] = x
medianOfThree cmp xs = median3 cmp first mid lastElem
  where
    first = head xs
    lastElem = last xs
    mid = xs !! (length xs `div` 2)

-- | Find median of three values
median3 :: (a -> a -> Ordering) -> a -> a -> a -> a
median3 cmp a b c
  | (cmp a b /= GT && cmp b c /= GT) || (cmp c b /= GT && cmp b a /= GT) = b
  | (cmp b a /= GT && cmp a c /= GT) || (cmp c a /= GT && cmp a b /= GT) = a
  | otherwise = c
