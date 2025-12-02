module AlgorithmsSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

import qualified Algorithms.BinarySearch as BS
import qualified Algorithms.InsertionSort as IS
import qualified Algorithms.MergeSort as MS
import qualified Algorithms.QuickSort as QS
import qualified Algorithms.Graph as G
import qualified Algorithms.BFS as BFS
import qualified Algorithms.DFS as DFS

spec :: Spec
spec = do
  describe "BinarySearch" $ do
    it "finds existing elements" $ do
      let vec = V.fromList [1, 3, 5, 7, 9, 11, 13]
      BS.search vec 7 `shouldBe` Just 3
      BS.search vec 1 `shouldBe` Just 0
      BS.search vec 13 `shouldBe` Just 6

    it "returns Nothing for non-existing elements" $ do
      let vec = V.fromList [1, 3, 5, 7, 9]
      BS.search vec 6 `shouldBe` Nothing

    it "lowerBound finds first >= target" $ do
      let vec = V.fromList [1, 2, 2, 2, 3, 4]
      BS.lowerBound vec 2 `shouldBe` 1

    it "upperBound finds first > target" $ do
      let vec = V.fromList [1, 2, 2, 2, 3, 4]
      BS.upperBound vec 2 `shouldBe` 4

  describe "InsertionSort" $ do
    it "sorts a list" $ do
      IS.sort [5, 2, 8, 1, 9, 3] `shouldBe` [1, 2, 3, 5, 8, 9]

    it "handles empty list" $ do
      IS.sort ([] :: [Int]) `shouldBe` []

    it "handles already sorted list" $ do
      IS.sort [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3, 4, 5]

    it "handles list with duplicates" $ do
      IS.sort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9]

  describe "MergeSort" $ do
    it "sorts a list" $ do
      MS.sort [5, 2, 8, 1, 9, 3] `shouldBe` [1, 2, 3, 5, 8, 9]

    it "handles empty list" $ do
      MS.sort ([] :: [Int]) `shouldBe` []

    it "handles single element" $ do
      MS.sort [42] `shouldBe` [42]

    it "handles list with duplicates" $ do
      MS.sort [3, 1, 4, 1, 5, 9, 2, 6, 5] `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9]

  describe "QuickSort" $ do
    it "sorts a list" $ do
      QS.sort [5, 2, 8, 1, 9, 3] `shouldBe` [1, 2, 3, 5, 8, 9]

    it "handles reverse sorted list" $ do
      QS.sort [9, 8, 7, 6, 5, 4, 3, 2, 1] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]

  describe "Graph" $ do
    it "adds vertices and edges" $ do
      let g = G.addEdge 1 2 $ G.addVertex 3 G.undirected
      G.hasVertex 1 g `shouldBe` True
      G.hasVertex 3 g `shouldBe` True
      G.hasEdge 1 2 g `shouldBe` True
      G.hasEdge 2 1 g `shouldBe` True  -- undirected

    it "directed graph has one-way edges" $ do
      let g = G.addEdge 1 2 G.directed
      G.hasEdge 1 2 g `shouldBe` True
      G.hasEdge 2 1 g `shouldBe` False

  describe "BFS" $ do
    it "traverses all reachable vertices" $ do
      let g = G.addEdge 2 4 $ G.addEdge 1 3 $ G.addEdge 1 2 G.undirected
      let result = BFS.traverse g 1
      length result `shouldBe` 4
      head result `shouldBe` 1

    it "finds shortest path" $ do
      let g = G.addEdge 1 3 $ G.addEdge 2 3 $ G.addEdge 1 2 G.undirected
      BFS.shortestPath g 1 3 `shouldBe` Just [1, 3]

    it "returns Nothing for unreachable vertices" $ do
      let g = G.addVertex 4 $ G.addEdge 1 2 G.undirected
      BFS.shortestPath g 1 4 `shouldBe` Nothing

    it "calculates distances correctly" $ do
      let g = G.addEdge 1 4 $ G.addEdge 2 3 $ G.addEdge 1 2 G.undirected
      let distances = BFS.distances g 1
      M.lookup 1 distances `shouldBe` Just 0
      M.lookup 2 distances `shouldBe` Just 1
      M.lookup 3 distances `shouldBe` Just 2
      M.lookup 4 distances `shouldBe` Just 1

  describe "DFS" $ do
    it "traverses all reachable vertices" $ do
      let g = G.addEdge 2 4 $ G.addEdge 1 3 $ G.addEdge 1 2 G.undirected
      let result = DFS.traverse g 1
      length result `shouldBe` 4
      head result `shouldBe` 1

    it "finds a path between vertices" $ do
      let g = G.addEdge 3 4 $ G.addEdge 2 3 $ G.addEdge 1 2 G.undirected
      let path = DFS.findPath g 1 4
      path `shouldSatisfy` \case
        Nothing -> False
        Just p -> head p == 1 && last p == 4

    it "detects cycles in directed graph" $ do
      let cyclic = G.addEdge 3 1 $ G.addEdge 2 3 $ G.addEdge 1 2 G.directed
      DFS.hasCycle cyclic `shouldBe` True

      let acyclic = G.addEdge 2 3 $ G.addEdge 1 2 G.directed
      DFS.hasCycle acyclic `shouldBe` False

    it "topological sort returns valid ordering" $ do
      let dag = G.addEdge 3 4 $ G.addEdge 2 4 $ G.addEdge 1 3 $ G.addEdge 1 2 G.directed
      case DFS.topologicalSort dag of
        Nothing -> expectationFailure "should return valid sort"
        Just sorted -> do
          let indexOf x = length $ takeWhile (/= x) sorted
          indexOf 1 `shouldSatisfy` (< indexOf 2)
          indexOf 1 `shouldSatisfy` (< indexOf 3)
          indexOf 2 `shouldSatisfy` (< indexOf 4)
          indexOf 3 `shouldSatisfy` (< indexOf 4)

    it "topological sort returns Nothing for cyclic graph" $ do
      let cyclic = G.addEdge 3 1 $ G.addEdge 2 3 $ G.addEdge 1 2 G.directed
      DFS.topologicalSort cyclic `shouldBe` Nothing

    it "topological sort returns Nothing for undirected graph" $ do
      DFS.topologicalSort G.undirected `shouldBe` Nothing
