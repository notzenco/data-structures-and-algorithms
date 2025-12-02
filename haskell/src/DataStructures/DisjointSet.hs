{-|
Module      : DataStructures.DisjointSet
Description : Disjoint set (Union-Find) implementation
Stability   : stable

A purely functional disjoint set using path compression.
Time: O(log n) for operations (without mutation, can't achieve inverse Ackermann)
Space: O(n)
-}
module DataStructures.DisjointSet
  ( DisjointSet
  , empty
  , makeSet
  , find
  , union
  , connected
  , contains
  , setCount
  , size
  ) where

import qualified Data.Map.Strict as M

-- | A disjoint set with parent and rank maps
data DisjointSet a = DisjointSet
  { parent :: M.Map a a
  , ranks  :: M.Map a Int
  } deriving (Show, Eq)

-- | Create an empty disjoint set
empty :: DisjointSet a
empty = DisjointSet M.empty M.empty

-- | Create a new set containing only the given element
makeSet :: Ord a => a -> DisjointSet a -> DisjointSet a
makeSet x ds
  | M.member x (parent ds) = ds
  | otherwise = DisjointSet
      { parent = M.insert x x (parent ds)
      , ranks = M.insert x 0 (ranks ds)
      }

-- | Find the representative of the set containing x
-- Returns the representative and updated disjoint set with path compression
find :: Ord a => a -> DisjointSet a -> Maybe (a, DisjointSet a)
find x ds = case M.lookup x (parent ds) of
  Nothing -> Nothing
  Just p
    | p == x    -> Just (x, ds)
    | otherwise -> do
        (root, ds') <- find p ds
        let ds'' = ds' { parent = M.insert x root (parent ds') }
        Just (root, ds'')

-- | Find without updating (simpler but no path compression)
findRoot :: Ord a => a -> DisjointSet a -> Maybe a
findRoot x ds = case M.lookup x (parent ds) of
  Nothing -> Nothing
  Just p
    | p == x    -> Just x
    | otherwise -> findRoot p ds

-- | Union two sets
union :: Ord a => a -> a -> DisjointSet a -> Maybe (DisjointSet a)
union x y ds = do
  (rootX, ds1) <- find x ds
  (rootY, ds2) <- find y ds1
  if rootX == rootY
    then Just ds2
    else do
      let rankX = M.findWithDefault 0 rootX (ranks ds2)
          rankY = M.findWithDefault 0 rootY (ranks ds2)
      Just $ if rankX < rankY
        then ds2 { parent = M.insert rootX rootY (parent ds2) }
        else if rankX > rankY
          then ds2 { parent = M.insert rootY rootX (parent ds2) }
          else ds2
            { parent = M.insert rootY rootX (parent ds2)
            , ranks = M.insert rootX (rankX + 1) (ranks ds2)
            }

-- | Check if two elements are in the same set
connected :: Ord a => a -> a -> DisjointSet a -> Bool
connected x y ds = case (findRoot x ds, findRoot y ds) of
  (Just rx, Just ry) -> rx == ry
  _                  -> False

-- | Check if element exists in any set
contains :: Ord a => a -> DisjointSet a -> Bool
contains x ds = M.member x (parent ds)

-- | Count the number of disjoint sets
setCount :: Ord a => DisjointSet a -> Int
setCount ds = length $ filter isRoot $ M.keys (parent ds)
  where
    isRoot x = M.lookup x (parent ds) == Just x

-- | Get total number of elements
size :: DisjointSet a -> Int
size ds = M.size (parent ds)
