{-|
Module      : Algorithms.InsertionSort
Description : Insertion sort algorithm
Stability   : stable

A purely functional insertion sort implementation.
Time: O(n²)
Space: O(n) - creates new list
-}
module Algorithms.InsertionSort
  ( sort
  , sortBy
  ) where

-- | Sort a list using insertion sort
sort :: Ord a => [a] -> [a]
sort = foldr insertSorted []

-- | Sort with custom comparison function
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldr (insertSortedBy cmp) []

-- | Insert element into sorted list
insertSorted :: Ord a => a -> [a] -> [a]
insertSorted x [] = [x]
insertSorted x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insertSorted x ys

-- | Insert element with custom comparison
insertSortedBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertSortedBy _ x [] = [x]
insertSortedBy cmp x (y:ys)
  | cmp x y /= GT = x : y : ys
  | otherwise     = y : insertSortedBy cmp x ys
