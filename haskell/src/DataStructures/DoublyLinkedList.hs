{-|
Module      : DataStructures.DoublyLinkedList
Description : Doubly linked list implementation using zipper pattern
Stability   : stable

A purely functional doubly linked list using the zipper pattern.
Time: O(1) for operations at focus, O(n) for traversal
Space: O(n)
-}
module DataStructures.DoublyLinkedList
  ( DoublyLinkedList
  , empty
  , isEmpty
  , prepend
  , append
  , removeFirst
  , removeLast
  , first
  , last
  , get
  , indexOf
  , contains
  , size
  , fromList
  , toList
  ) where

import Prelude hiding (last)

-- | A doubly linked list represented as two lists (for functional immutability)
-- The first list is reversed (for O(1) prepend), second is normal (for O(1) append)
data DoublyLinkedList a = DoublyLinkedList [a] [a] Int
  deriving (Show, Eq)

-- | Create an empty list
empty :: DoublyLinkedList a
empty = DoublyLinkedList [] [] 0

-- | Check if list is empty
isEmpty :: DoublyLinkedList a -> Bool
isEmpty (DoublyLinkedList _ _ n) = n == 0

-- | Add element to the front
prepend :: a -> DoublyLinkedList a -> DoublyLinkedList a
prepend x (DoublyLinkedList front back n) = DoublyLinkedList (x : front) back (n + 1)

-- | Add element to the back
append :: a -> DoublyLinkedList a -> DoublyLinkedList a
append x (DoublyLinkedList front back n) = DoublyLinkedList front (x : back) (n + 1)

-- | Remove first element
removeFirst :: DoublyLinkedList a -> Maybe (a, DoublyLinkedList a)
removeFirst (DoublyLinkedList [] [] _) = Nothing
removeFirst (DoublyLinkedList [] back n) =
  removeFirst (DoublyLinkedList (reverse back) [] n)
removeFirst (DoublyLinkedList (x:xs) back n) =
  Just (x, DoublyLinkedList xs back (n - 1))

-- | Remove last element
removeLast :: DoublyLinkedList a -> Maybe (a, DoublyLinkedList a)
removeLast (DoublyLinkedList [] [] _) = Nothing
removeLast (DoublyLinkedList front [] n) =
  removeLast (DoublyLinkedList [] (reverse front) n)
removeLast (DoublyLinkedList front (x:xs) n) =
  Just (x, DoublyLinkedList front xs (n - 1))

-- | Get first element
first :: DoublyLinkedList a -> Maybe a
first dll = fst <$> removeFirst dll

-- | Get last element
last :: DoublyLinkedList a -> Maybe a
last dll = fst <$> removeLast dll

-- | Get element at index
get :: Int -> DoublyLinkedList a -> Maybe a
get i dll
  | i < 0 || i >= size dll = Nothing
  | otherwise = Just $ toList dll !! i

-- | Find index of first occurrence
indexOf :: Eq a => a -> DoublyLinkedList a -> Maybe Int
indexOf x dll = findIndex' 0 (toList dll)
  where
    findIndex' _ []     = Nothing
    findIndex' n (y:ys)
      | x == y    = Just n
      | otherwise = findIndex' (n + 1) ys

-- | Check if element exists
contains :: Eq a => a -> DoublyLinkedList a -> Bool
contains x dll = x `elem` toList dll

-- | Get the number of elements
size :: DoublyLinkedList a -> Int
size (DoublyLinkedList _ _ n) = n

-- | Create from list
fromList :: [a] -> DoublyLinkedList a
fromList xs = DoublyLinkedList xs [] (length xs)

-- | Convert to list
toList :: DoublyLinkedList a -> [a]
toList (DoublyLinkedList front back _) = front ++ reverse back
