{-|
Module      : Algorithms.MergeSort
Description : Merge sort algorithm
Stability   : stable

A purely functional merge sort implementation.
Time: O(n log n)
Space: O(n)
-}
module Algorithms.MergeSort
  ( sort
  , sortBy
  ) where

-- | Sort a list using merge sort
sort :: Ord a => [a] -> [a]
sort = sortBy compare

-- | Sort with custom comparison function
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _   []  = []
sortBy _   [x] = [x]
sortBy cmp xs  = merge cmp (sortBy cmp left) (sortBy cmp right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

-- | Merge two sorted lists
merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge cmp (x:xs) (y:ys)
  | cmp x y /= GT = x : merge cmp xs (y:ys)
  | otherwise     = y : merge cmp (x:xs) ys
