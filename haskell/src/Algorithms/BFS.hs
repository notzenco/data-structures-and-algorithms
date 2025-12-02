{-|
Module      : Algorithms.BFS
Description : Breadth-first search algorithms
Stability   : stable

Breadth-first search algorithms for graphs.
Time: O(V + E)
Space: O(V)
-}
module Algorithms.BFS
  ( traverse
  , shortestPath
  , distances
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Algorithms.Graph (Graph, neighbors, hasVertex)

-- | BFS traversal from a starting vertex
traverse :: Ord a => Graph a -> a -> [a]
traverse g start
  | not (hasVertex start g) = []
  | otherwise = go (S.singleton start) [start] []
  where
    go _ [] result = reverse result
    go visited (v:queue) result =
      let newNeighbors = filter (`S.notMember` visited) $ S.toList $ neighbors v g
          visited' = foldr S.insert visited newNeighbors
      in go visited' (queue ++ newNeighbors) (v : result)

-- | Find shortest path between two vertices
shortestPath :: Ord a => Graph a -> a -> a -> Maybe [a]
shortestPath g start end
  | not (hasVertex start g) || not (hasVertex end g) = Nothing
  | start == end = Just [start]
  | otherwise = go (S.singleton start) [(start, [start])]
  where
    go _ [] = Nothing
    go visited ((v, path):queue)
      | v == end = Just (reverse path)
      | otherwise =
          let newNeighbors = filter (`S.notMember` visited) $ S.toList $ neighbors v g
              visited' = foldr S.insert visited newNeighbors
              newPaths = [(n, n : path) | n <- newNeighbors]
          in go visited' (queue ++ newPaths)

-- | Calculate distances from start vertex to all reachable vertices
distances :: Ord a => Graph a -> a -> M.Map a Int
distances g start
  | not (hasVertex start g) = M.empty
  | otherwise = go (S.singleton start) [(start, 0)] (M.singleton start 0)
  where
    go _ [] result = result
    go visited ((v, dist):queue) result =
      let newNeighbors = filter (`S.notMember` visited) $ S.toList $ neighbors v g
          visited' = foldr S.insert visited newNeighbors
          newDist = dist + 1
          result' = foldr (\n -> M.insert n newDist) result newNeighbors
          newQueue = [(n, newDist) | n <- newNeighbors]
      in go visited' (queue ++ newQueue) result'
