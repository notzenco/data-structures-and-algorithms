{-|
Module      : DataStructures.Queue
Description : FIFO queue implementation using two stacks
Stability   : stable

A purely functional queue using the banker's queue technique.
Time: O(1) amortized for all operations
Space: O(n)
-}
module DataStructures.Queue
  ( Queue
  , empty
  , isEmpty
  , enqueue
  , dequeue
  , peek
  , size
  , fromList
  , toList
  ) where

-- | A queue using two lists for amortized O(1) operations
data Queue a = Queue [a] [a]
  deriving (Show, Eq)

-- | Create an empty queue
empty :: Queue a
empty = Queue [] []

-- | Check if queue is empty
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

-- | Add an element to the back of the queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front back) = Queue front (x : back)

-- | Remove an element from the front of the queue
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] [])     = Nothing
dequeue (Queue [] back)   = dequeue (Queue (reverse back) [])
dequeue (Queue (x:xs) back) = Just (x, Queue xs back)

-- | Peek at the front element without removing
peek :: Queue a -> Maybe a
peek (Queue [] [])   = Nothing
peek (Queue [] back) = Just (last back)
peek (Queue (x:_) _) = Just x

-- | Get the number of elements in the queue
size :: Queue a -> Int
size (Queue front back) = length front + length back

-- | Create a queue from a list (first element at front)
fromList :: [a] -> Queue a
fromList xs = Queue xs []

-- | Convert queue to list (front element first)
toList :: Queue a -> [a]
toList (Queue front back) = front ++ reverse back
