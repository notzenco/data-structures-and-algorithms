module DataStructuresSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V

import qualified DataStructures.Stack as Stack
import qualified DataStructures.Queue as Queue
import qualified DataStructures.DynamicArray as DArray
import qualified DataStructures.SinglyLinkedList as SLL
import qualified DataStructures.DoublyLinkedList as DLL
import qualified DataStructures.Deque as Deque
import qualified DataStructures.HashTable as HT
import qualified DataStructures.BinarySearchTree as BST
import qualified DataStructures.MinHeap as Heap
import qualified DataStructures.DisjointSet as DS

spec :: Spec
spec = do
  describe "Stack" $ do
    it "push and pop work correctly" $ do
      let stack = Stack.push 3 $ Stack.push 2 $ Stack.push 1 Stack.empty
      Stack.pop stack `shouldBe` Just (3, Stack.push 2 $ Stack.push 1 Stack.empty)

    it "peek returns top element without removing" $ do
      let stack = Stack.push 42 Stack.empty
      Stack.peek stack `shouldBe` Just 42
      Stack.size stack `shouldBe` 1

    it "isEmpty returns correct value" $ do
      Stack.isEmpty Stack.empty `shouldBe` True
      Stack.isEmpty (Stack.push 1 Stack.empty) `shouldBe` False

  describe "Queue" $ do
    it "enqueue and dequeue work correctly" $ do
      let queue = Queue.enqueue 3 $ Queue.enqueue 2 $ Queue.enqueue 1 Queue.empty
      fmap fst (Queue.dequeue queue) `shouldBe` Just 1

    it "maintains FIFO order" $ do
      let queue = Queue.enqueue 3 $ Queue.enqueue 2 $ Queue.enqueue 1 Queue.empty
      Queue.toList queue `shouldBe` [1, 2, 3]

  describe "DynamicArray" $ do
    it "push and pop work correctly" $ do
      let arr = DArray.push 3 $ DArray.push 2 $ DArray.push 1 DArray.empty
      fmap fst (DArray.pop arr) `shouldBe` Just 3

    it "get and set work correctly" $ do
      let arr = DArray.fromList [1, 2, 3]
      DArray.get 1 arr `shouldBe` Just 2
      DArray.toList <$> DArray.set 1 42 arr `shouldBe` Just [1, 42, 3]

  describe "SinglyLinkedList" $ do
    it "prepend and append work correctly" $ do
      let list = SLL.append 3 $ SLL.prepend 1 $ SLL.append 2 SLL.empty
      SLL.toList list `shouldBe` [1, 2, 3]

    it "indexOf finds elements" $ do
      let list = SLL.fromList [10, 20, 30]
      SLL.indexOf 20 list `shouldBe` Just 1
      SLL.indexOf 100 list `shouldBe` Nothing

  describe "DoublyLinkedList" $ do
    it "prepend and append work correctly" $ do
      let list = DLL.append 3 $ DLL.prepend 1 $ DLL.append 2 DLL.empty
      DLL.toList list `shouldBe` [1, 2, 3]

    it "first and last work correctly" $ do
      let list = DLL.fromList [1, 2, 3]
      DLL.first list `shouldBe` Just 1
      DLL.last list `shouldBe` Just 3

  describe "Deque" $ do
    it "pushFront and pushBack work correctly" $ do
      let deque = Deque.pushBack 3 $ Deque.pushFront 1 $ Deque.pushBack 2 Deque.empty
      Deque.toList deque `shouldBe` [1, 2, 3]

    it "popFront and popBack work correctly" $ do
      let deque = Deque.fromList [1, 2, 3]
      fmap fst (Deque.popFront deque) `shouldBe` Just 1
      fmap fst (Deque.popBack deque) `shouldBe` Just 3

  describe "HashTable" $ do
    it "put and get work correctly" $ do
      let table = HT.put "one" 1 $ HT.put "two" 2 HT.empty
      HT.get "one" table `shouldBe` Just 1
      HT.get "three" table `shouldBe` Nothing

    it "remove works correctly" $ do
      let table = HT.put "key" 42 HT.empty
      let (val, table') = HT.remove "key" table
      val `shouldBe` Just 42
      HT.get "key" table' `shouldBe` Nothing

  describe "BinarySearchTree" $ do
    it "insert and contains work correctly" $ do
      let bst = BST.fromList [5, 3, 7, 1, 9]
      BST.contains 5 bst `shouldBe` True
      BST.contains 10 bst `shouldBe` False

    it "findMin and findMax work correctly" $ do
      let bst = BST.fromList [5, 3, 7, 1, 9]
      BST.findMin bst `shouldBe` Just 1
      BST.findMax bst `shouldBe` Just 9

    it "inorder traversal returns sorted list" $ do
      let bst = BST.fromList [5, 3, 7, 1, 9]
      BST.inorder bst `shouldBe` [1, 3, 5, 7, 9]

    it "remove works correctly" $ do
      let bst = BST.remove 5 $ BST.fromList [5, 3, 7]
      BST.contains 5 bst `shouldBe` False
      BST.size bst `shouldBe` 2

  describe "MinHeap" $ do
    it "insert and extractMin work correctly" $ do
      let heap = Heap.fromList [5, 3, 7, 1, 9]
      fmap fst (Heap.extractMin heap) `shouldBe` Just 1

    it "toList returns sorted order" $ do
      let heap = Heap.fromList [5, 3, 7, 1, 9]
      Heap.toList heap `shouldBe` [1, 3, 5, 7, 9]

    it "peek returns minimum without removing" $ do
      let heap = Heap.fromList [5, 3, 7]
      Heap.peek heap `shouldBe` Just 3
      Heap.size heap `shouldBe` 3

  describe "DisjointSet" $ do
    it "makeSet and find work correctly" $ do
      let ds = DS.makeSet 1 $ DS.makeSet 2 DS.empty
      fmap fst (DS.find 1 ds) `shouldBe` Just 1
      fmap fst (DS.find 3 ds) `shouldBe` Nothing

    it "union and connected work correctly" $ do
      let ds = DS.makeSet 3 $ DS.makeSet 2 $ DS.makeSet 1 DS.empty
      DS.connected 1 2 ds `shouldBe` False
      case DS.union 1 2 ds of
        Nothing -> expectationFailure "union should succeed"
        Just ds' -> DS.connected 1 2 ds' `shouldBe` True

    it "setCount tracks disjoint sets" $ do
      let ds = DS.makeSet 4 $ DS.makeSet 3 $ DS.makeSet 2 $ DS.makeSet 1 DS.empty
      DS.setCount ds `shouldBe` 4
      case DS.union 1 2 ds >>= DS.union 3 4 of
        Nothing -> expectationFailure "unions should succeed"
        Just ds' -> DS.setCount ds' `shouldBe` 2
