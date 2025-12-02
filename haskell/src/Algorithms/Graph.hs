{-|
Module      : Algorithms.Graph
Description : Graph data structure using adjacency list
Stability   : stable

A purely functional graph implementation using adjacency list.
-}
module Algorithms.Graph
  ( Graph
  , empty
  , directed
  , undirected
  , addVertex
  , addEdge
  , removeVertex
  , removeEdge
  , hasVertex
  , hasEdge
  , neighbors
  , vertices
  , vertexCount
  , edgeCount
  , isDirected
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | A graph represented as an adjacency list
data Graph a = Graph
  { adjacencyList :: M.Map a (S.Set a)
  , graphDirected :: Bool
  } deriving (Show, Eq)

-- | Create an empty directed graph
empty :: Graph a
empty = Graph M.empty True

-- | Create an empty directed graph
directed :: Graph a
directed = Graph M.empty True

-- | Create an empty undirected graph
undirected :: Graph a
undirected = Graph M.empty False

-- | Add a vertex to the graph
addVertex :: Ord a => a -> Graph a -> Graph a
addVertex v g = g { adjacencyList = M.insertWith S.union v S.empty (adjacencyList g) }

-- | Add an edge to the graph
addEdge :: Ord a => a -> a -> Graph a -> Graph a
addEdge from to g =
  let g1 = addVertex from $ addVertex to g
      g2 = g1 { adjacencyList = M.adjust (S.insert to) from (adjacencyList g1) }
  in if graphDirected g
     then g2
     else g2 { adjacencyList = M.adjust (S.insert from) to (adjacencyList g2) }

-- | Remove a vertex and all its edges
removeVertex :: Ord a => a -> Graph a -> Graph a
removeVertex v g = g { adjacencyList = M.map (S.delete v) $ M.delete v (adjacencyList g) }

-- | Remove an edge
removeEdge :: Ord a => a -> a -> Graph a -> Graph a
removeEdge from to g =
  let g1 = g { adjacencyList = M.adjust (S.delete to) from (adjacencyList g) }
  in if graphDirected g
     then g1
     else g1 { adjacencyList = M.adjust (S.delete from) to (adjacencyList g1) }

-- | Check if vertex exists
hasVertex :: Ord a => a -> Graph a -> Bool
hasVertex v g = M.member v (adjacencyList g)

-- | Check if edge exists
hasEdge :: Ord a => a -> a -> Graph a -> Bool
hasEdge from to g = case M.lookup from (adjacencyList g) of
  Nothing   -> False
  Just neighbors' -> S.member to neighbors'

-- | Get neighbors of a vertex
neighbors :: Ord a => a -> Graph a -> S.Set a
neighbors v g = M.findWithDefault S.empty v (adjacencyList g)

-- | Get all vertices
vertices :: Graph a -> [a]
vertices = M.keys . adjacencyList

-- | Get number of vertices
vertexCount :: Graph a -> Int
vertexCount = M.size . adjacencyList

-- | Get number of edges
edgeCount :: Graph a -> Int
edgeCount g =
  let total = sum $ map S.size $ M.elems (adjacencyList g)
  in if graphDirected g then total else total `div` 2

-- | Check if graph is directed
isDirected :: Graph a -> Bool
isDirected = graphDirected
