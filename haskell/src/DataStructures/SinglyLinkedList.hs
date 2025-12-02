{-|
Module      : DataStructures.SinglyLinkedList
Description : Singly linked list implementation
Stability   : stable

A purely functional singly linked list.
Time: O(1) for prepend, O(n) for other operations
Space: O(n)
-}
module DataStructures.SinglyLinkedList
  ( SinglyLinkedList
  , empty
  , isEmpty
  , prepend
  , append
  , insert
  , removeAt
  , get
  , indexOf
  , contains
  , size
  , fromList
  , toList
  ) where

-- | A singly linked list (essentially Haskell's built-in list)
newtype SinglyLinkedList a = SinglyLinkedList [a]
  deriving (Show, Eq)

-- | Create an empty list
empty :: SinglyLinkedList a
empty = SinglyLinkedList []

-- | Check if list is empty
isEmpty :: SinglyLinkedList a -> Bool
isEmpty (SinglyLinkedList xs) = null xs

-- | Add element to the front
prepend :: a -> SinglyLinkedList a -> SinglyLinkedList a
prepend x (SinglyLinkedList xs) = SinglyLinkedList (x : xs)

-- | Add element to the back
append :: a -> SinglyLinkedList a -> SinglyLinkedList a
append x (SinglyLinkedList xs) = SinglyLinkedList (xs ++ [x])

-- | Insert element at index
insert :: Int -> a -> SinglyLinkedList a -> Maybe (SinglyLinkedList a)
insert i x (SinglyLinkedList xs)
  | i < 0 || i > length xs = Nothing
  | otherwise = Just $ SinglyLinkedList $ take i xs ++ [x] ++ drop i xs

-- | Remove element at index
removeAt :: Int -> SinglyLinkedList a -> Maybe (a, SinglyLinkedList a)
removeAt i (SinglyLinkedList xs)
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i, SinglyLinkedList $ take i xs ++ drop (i + 1) xs)

-- | Get element at index
get :: Int -> SinglyLinkedList a -> Maybe a
get i (SinglyLinkedList xs)
  | i < 0 || i >= length xs = Nothing
  | otherwise               = Just (xs !! i)

-- | Find index of first occurrence
indexOf :: Eq a => a -> SinglyLinkedList a -> Maybe Int
indexOf x (SinglyLinkedList xs) = findIndex' 0 xs
  where
    findIndex' _ []     = Nothing
    findIndex' n (y:ys)
      | x == y    = Just n
      | otherwise = findIndex' (n + 1) ys

-- | Check if element exists
contains :: Eq a => a -> SinglyLinkedList a -> Bool
contains x (SinglyLinkedList xs) = x `elem` xs

-- | Get the number of elements
size :: SinglyLinkedList a -> Int
size (SinglyLinkedList xs) = length xs

-- | Create from list
fromList :: [a] -> SinglyLinkedList a
fromList = SinglyLinkedList

-- | Convert to list
toList :: SinglyLinkedList a -> [a]
toList (SinglyLinkedList xs) = xs
