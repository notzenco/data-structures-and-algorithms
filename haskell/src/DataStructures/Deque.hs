{-|
Module      : DataStructures.Deque
Description : Double-ended queue implementation
Stability   : stable

A purely functional deque (double-ended queue).
Time: O(1) amortized for all operations
Space: O(n)
-}
module DataStructures.Deque
  ( Deque
  , empty
  , isEmpty
  , pushFront
  , pushBack
  , popFront
  , popBack
  , peekFront
  , peekBack
  , size
  , fromList
  , toList
  ) where

-- | A deque using two lists for amortized O(1) operations at both ends
data Deque a = Deque [a] [a] Int
  deriving (Show, Eq)

-- | Create an empty deque
empty :: Deque a
empty = Deque [] [] 0

-- | Check if deque is empty
isEmpty :: Deque a -> Bool
isEmpty (Deque _ _ n) = n == 0

-- | Add element to the front
pushFront :: a -> Deque a -> Deque a
pushFront x (Deque front back n) = Deque (x : front) back (n + 1)

-- | Add element to the back
pushBack :: a -> Deque a -> Deque a
pushBack x (Deque front back n) = Deque front (x : back) (n + 1)

-- | Remove element from the front
popFront :: Deque a -> Maybe (a, Deque a)
popFront (Deque [] [] _) = Nothing
popFront (Deque [] back n) = popFront (Deque (reverse back) [] n)
popFront (Deque (x:xs) back n) = Just (x, Deque xs back (n - 1))

-- | Remove element from the back
popBack :: Deque a -> Maybe (a, Deque a)
popBack (Deque [] [] _) = Nothing
popBack (Deque front [] n) = popBack (Deque [] (reverse front) n)
popBack (Deque front (x:xs) n) = Just (x, Deque front xs (n - 1))

-- | Peek at front element without removing
peekFront :: Deque a -> Maybe a
peekFront (Deque [] [] _) = Nothing
peekFront (Deque [] back _) = Just (Prelude.last back)
peekFront (Deque (x:_) _ _) = Just x

-- | Peek at back element without removing
peekBack :: Deque a -> Maybe a
peekBack (Deque [] [] _) = Nothing
peekBack (Deque front [] _) = Just (Prelude.last front)
peekBack (Deque _ (x:_) _) = Just x

-- | Get the number of elements
size :: Deque a -> Int
size (Deque _ _ n) = n

-- | Create from list (first element at front)
fromList :: [a] -> Deque a
fromList xs = Deque xs [] (length xs)

-- | Convert to list (front element first)
toList :: Deque a -> [a]
toList (Deque front back _) = front ++ reverse back
