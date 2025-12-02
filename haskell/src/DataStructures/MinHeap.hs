{-|
Module      : DataStructures.MinHeap
Description : Min-heap (priority queue) implementation
Stability   : stable

A purely functional min-heap implementation.
Time: O(log n) for insert/extract, O(1) for peek
Space: O(n)
-}
module DataStructures.MinHeap
  ( MinHeap
  , empty
  , isEmpty
  , insert
  , extractMin
  , peek
  , size
  , fromList
  , toList
  ) where

-- | A min-heap represented as a leftist heap
data MinHeap a = Empty | Node Int a (MinHeap a) (MinHeap a)
  deriving (Show, Eq)

-- | Get rank of heap (length of right spine)
rank :: MinHeap a -> Int
rank Empty          = 0
rank (Node r _ _ _) = r

-- | Create an empty heap
empty :: MinHeap a
empty = Empty

-- | Check if heap is empty
isEmpty :: MinHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Make a node with correct rank (leftist property)
makeNode :: a -> MinHeap a -> MinHeap a -> MinHeap a
makeNode x left right
  | rank left >= rank right = Node (rank right + 1) x left right
  | otherwise               = Node (rank left + 1) x right left

-- | Merge two heaps
merge :: Ord a => MinHeap a -> MinHeap a -> MinHeap a
merge Empty h = h
merge h Empty = h
merge h1@(Node _ x1 l1 r1) h2@(Node _ x2 l2 r2)
  | x1 <= x2  = makeNode x1 l1 (merge r1 h2)
  | otherwise = makeNode x2 l2 (merge h1 r2)

-- | Insert a value
insert :: Ord a => a -> MinHeap a -> MinHeap a
insert x h = merge (Node 1 x Empty Empty) h

-- | Extract the minimum value
extractMin :: Ord a => MinHeap a -> Maybe (a, MinHeap a)
extractMin Empty          = Nothing
extractMin (Node _ x l r) = Just (x, merge l r)

-- | Peek at minimum without removing
peek :: MinHeap a -> Maybe a
peek Empty          = Nothing
peek (Node _ x _ _) = Just x

-- | Get number of elements
size :: MinHeap a -> Int
size Empty            = 0
size (Node _ _ l r) = 1 + size l + size r

-- | Create heap from list
fromList :: Ord a => [a] -> MinHeap a
fromList = foldr insert empty

-- | Convert to sorted list
toList :: Ord a => MinHeap a -> [a]
toList h = case extractMin h of
  Nothing      -> []
  Just (x, h') -> x : toList h'
