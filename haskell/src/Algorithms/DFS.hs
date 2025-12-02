{-|
Module      : Algorithms.DFS
Description : Depth-first search algorithms
Stability   : stable

Depth-first search algorithms for graphs.
Time: O(V + E)
Space: O(V)
-}
module Algorithms.DFS
  ( traverse
  , traverseRecursive
  , findPath
  , hasCycle
  , topologicalSort
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Algorithms.Graph (Graph, neighbors, hasVertex, vertices, isDirected)

-- | DFS traversal from a starting vertex (iterative)
traverse :: Ord a => Graph a -> a -> [a]
traverse g start
  | not (hasVertex start g) = []
  | otherwise = go S.empty [start] []
  where
    go _ [] result = reverse result
    go visited (v:stack) result
      | S.member v visited = go visited stack result
      | otherwise =
          let visited' = S.insert v visited
              newNeighbors = filter (`S.notMember` visited') $ S.toList $ neighbors v g
          in go visited' (newNeighbors ++ stack) (v : result)

-- | DFS traversal from a starting vertex (recursive)
traverseRecursive :: Ord a => Graph a -> a -> [a]
traverseRecursive g start
  | not (hasVertex start g) = []
  | otherwise = go S.empty start
  where
    go visited v
      | S.member v visited = []
      | otherwise =
          let visited' = S.insert v visited
              neighborsList = S.toList $ neighbors v g
          in v : concatMap (go visited') neighborsList

-- | Find a path between two vertices using DFS
findPath :: Ord a => Graph a -> a -> a -> Maybe [a]
findPath g start end
  | not (hasVertex start g) || not (hasVertex end g) = Nothing
  | start == end = Just [start]
  | otherwise = go S.empty [(start, [start])]
  where
    go _ [] = Nothing
    go visited ((v, path):stack)
      | v == end = Just (reverse path)
      | S.member v visited = go visited stack
      | otherwise =
          let visited' = S.insert v visited
              newNeighbors = filter (`S.notMember` visited') $ S.toList $ neighbors v g
              newPaths = [(n, n : path) | n <- newNeighbors]
          in go visited' (newPaths ++ stack)

-- | Check if graph has a cycle
hasCycle :: Ord a => Graph a -> Bool
hasCycle g = any (hasCycleFrom S.empty S.empty) (vertices g)
  where
    hasCycleFrom visited recStack v
      | S.member v recStack = True
      | S.member v visited = False
      | otherwise =
          let visited' = S.insert v visited
              recStack' = S.insert v recStack
              neighborsList = S.toList $ neighbors v g
          in any (hasCycleFrom visited' recStack') neighborsList

-- | Topological sort (only for directed graphs)
topologicalSort :: Ord a => Graph a -> Maybe [a]
topologicalSort g
  | not (isDirected g) = Nothing
  | hasCycle g = Nothing
  | otherwise = Just $ go S.empty [] (vertices g)
  where
    go _ result [] = result
    go visited result (v:vs)
      | S.member v visited = go visited result vs
      | otherwise =
          let (visited', sorted) = dfs visited v
          in go visited' (sorted ++ result) vs

    dfs visited v
      | S.member v visited = (visited, [])
      | otherwise =
          let visited' = S.insert v visited
              neighborsList = S.toList $ neighbors v g
              (visited'', sorted) = foldl dfsNeighbor (visited', []) neighborsList
          in (visited'', v : sorted)

    dfsNeighbor (visited, acc) n =
      let (visited', sorted) = dfs visited n
      in (visited', sorted ++ acc)
