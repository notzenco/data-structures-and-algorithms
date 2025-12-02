{-|
Module      : DataStructures.Stack
Description : LIFO stack implementation
Stability   : stable

A purely functional stack implementation.
Time: O(1) for all operations
Space: O(n)
-}
module DataStructures.Stack
  ( Stack
  , empty
  , isEmpty
  , push
  , pop
  , peek
  , size
  , fromList
  , toList
  ) where

-- | A stack represented as a list (head is top)
newtype Stack a = Stack [a]
  deriving (Show, Eq)

-- | Create an empty stack
empty :: Stack a
empty = Stack []

-- | Check if stack is empty
isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = null xs

-- | Push an element onto the stack
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

-- | Pop an element from the stack
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack [])     = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

-- | Peek at the top element without removing
peek :: Stack a -> Maybe a
peek (Stack [])    = Nothing
peek (Stack (x:_)) = Just x

-- | Get the number of elements in the stack
size :: Stack a -> Int
size (Stack xs) = length xs

-- | Create a stack from a list (first element becomes top)
fromList :: [a] -> Stack a
fromList = Stack

-- | Convert stack to list (top element first)
toList :: Stack a -> [a]
toList (Stack xs) = xs
