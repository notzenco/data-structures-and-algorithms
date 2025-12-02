{-|
Module      : DataStructures.DynamicArray
Description : Dynamic array implementation using Data.Vector
Stability   : stable

A dynamic array wrapper around Data.Vector.
Time: O(1) for access, O(n) for modification (creates new vector)
Space: O(n)
-}
module DataStructures.DynamicArray
  ( DynamicArray
  , empty
  , isEmpty
  , push
  , pop
  , get
  , set
  , insert
  , removeAt
  , indexOf
  , contains
  , size
  , fromList
  , toList
  ) where

import qualified Data.Vector as V

-- | A dynamic array backed by Vector
newtype DynamicArray a = DynamicArray (V.Vector a)
  deriving (Show, Eq)

-- | Create an empty dynamic array
empty :: DynamicArray a
empty = DynamicArray V.empty

-- | Check if array is empty
isEmpty :: DynamicArray a -> Bool
isEmpty (DynamicArray v) = V.null v

-- | Add element to the end
push :: a -> DynamicArray a -> DynamicArray a
push x (DynamicArray v) = DynamicArray (V.snoc v x)

-- | Remove and return the last element
pop :: DynamicArray a -> Maybe (a, DynamicArray a)
pop (DynamicArray v)
  | V.null v  = Nothing
  | otherwise = Just (V.last v, DynamicArray (V.init v))

-- | Get element at index
get :: Int -> DynamicArray a -> Maybe a
get i (DynamicArray v)
  | i < 0 || i >= V.length v = Nothing
  | otherwise                = Just (v V.! i)

-- | Set element at index
set :: Int -> a -> DynamicArray a -> Maybe (DynamicArray a)
set i x (DynamicArray v)
  | i < 0 || i >= V.length v = Nothing
  | otherwise                = Just (DynamicArray (v V.// [(i, x)]))

-- | Insert element at index
insert :: Int -> a -> DynamicArray a -> Maybe (DynamicArray a)
insert i x (DynamicArray v)
  | i < 0 || i > V.length v = Nothing
  | otherwise = Just $ DynamicArray $
      V.take i v V.++ V.singleton x V.++ V.drop i v

-- | Remove element at index
removeAt :: Int -> DynamicArray a -> Maybe (a, DynamicArray a)
removeAt i (DynamicArray v)
  | i < 0 || i >= V.length v = Nothing
  | otherwise = Just (v V.! i, DynamicArray $ V.take i v V.++ V.drop (i + 1) v)

-- | Find index of first occurrence
indexOf :: Eq a => a -> DynamicArray a -> Maybe Int
indexOf x (DynamicArray v) = V.findIndex (== x) v

-- | Check if element exists
contains :: Eq a => a -> DynamicArray a -> Bool
contains x (DynamicArray v) = V.elem x v

-- | Get the number of elements
size :: DynamicArray a -> Int
size (DynamicArray v) = V.length v

-- | Create from list
fromList :: [a] -> DynamicArray a
fromList = DynamicArray . V.fromList

-- | Convert to list
toList :: DynamicArray a -> [a]
toList (DynamicArray v) = V.toList v
