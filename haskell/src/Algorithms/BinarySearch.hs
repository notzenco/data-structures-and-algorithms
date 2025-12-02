{-|
Module      : Algorithms.BinarySearch
Description : Binary search algorithms
Stability   : stable

Binary search algorithms for sorted sequences.
Time: O(log n)
Space: O(1) iterative, O(log n) recursive
-}
module Algorithms.BinarySearch
  ( search
  , lowerBound
  , upperBound
  , contains
  ) where

import qualified Data.Vector as V

-- | Search for a value in a sorted vector
search :: Ord a => V.Vector a -> a -> Maybe Int
search vec target = go 0 (V.length vec - 1)
  where
    go left right
      | left > right = Nothing
      | otherwise =
          let mid = left + (right - left) `div` 2
              midVal = vec V.! mid
          in case compare midVal target of
               EQ -> Just mid
               LT -> go (mid + 1) right
               GT -> go left (mid - 1)

-- | Find index of first element >= target
lowerBound :: Ord a => V.Vector a -> a -> Int
lowerBound vec target = go 0 (V.length vec)
  where
    go left right
      | left >= right = left
      | otherwise =
          let mid = left + (right - left) `div` 2
          in if vec V.! mid < target
             then go (mid + 1) right
             else go left mid

-- | Find index of first element > target
upperBound :: Ord a => V.Vector a -> a -> Int
upperBound vec target = go 0 (V.length vec)
  where
    go left right
      | left >= right = left
      | otherwise =
          let mid = left + (right - left) `div` 2
          in if vec V.! mid <= target
             then go (mid + 1) right
             else go left mid

-- | Check if value exists in sorted vector
contains :: Ord a => V.Vector a -> a -> Bool
contains vec target = case search vec target of
  Just _  -> True
  Nothing -> False
