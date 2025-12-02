{-|
Module      : DataStructures.BinarySearchTree
Description : Binary search tree implementation
Stability   : stable

A purely functional binary search tree.
Time: O(log n) average, O(n) worst case
Space: O(n)
-}
module DataStructures.BinarySearchTree
  ( BST
  , empty
  , isEmpty
  , insert
  , contains
  , remove
  , findMin
  , findMax
  , size
  , inorder
  , preorder
  , postorder
  , levelOrder
  , fromList
  , toList
  ) where

-- | A binary search tree
data BST a = Empty | Node a (BST a) (BST a)
  deriving (Show, Eq)

-- | Create an empty tree
empty :: BST a
empty = Empty

-- | Check if tree is empty
isEmpty :: BST a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Insert a value into the tree
insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node v left right)
  | x < v     = Node v (insert x left) right
  | x > v     = Node v left (insert x right)
  | otherwise = Node x left right  -- Replace existing value

-- | Check if value exists in tree
contains :: Ord a => a -> BST a -> Bool
contains _ Empty = False
contains x (Node v left right)
  | x < v     = contains x left
  | x > v     = contains x right
  | otherwise = True

-- | Remove a value from the tree
remove :: Ord a => a -> BST a -> BST a
remove _ Empty = Empty
remove x (Node v left right)
  | x < v     = Node v (remove x left) right
  | x > v     = Node v left (remove x right)
  | otherwise = removeNode (Node v left right)

-- | Helper to remove the current node
removeNode :: Ord a => BST a -> BST a
removeNode (Node _ Empty right) = right
removeNode (Node _ left Empty)  = left
removeNode (Node _ left right)  =
  let minVal = getMin right
  in Node minVal left (remove minVal right)
removeNode Empty = Empty

-- | Get minimum value (helper)
getMin :: BST a -> a
getMin (Node v Empty _) = v
getMin (Node _ left _)  = getMin left
getMin Empty            = error "Empty tree has no minimum"

-- | Find minimum value
findMin :: BST a -> Maybe a
findMin Empty           = Nothing
findMin (Node v Empty _) = Just v
findMin (Node _ left _)  = findMin left

-- | Find maximum value
findMax :: BST a -> Maybe a
findMax Empty            = Nothing
findMax (Node v _ Empty) = Just v
findMax (Node _ _ right) = findMax right

-- | Get number of nodes
size :: BST a -> Int
size Empty             = 0
size (Node _ left right) = 1 + size left + size right

-- | Inorder traversal (sorted order)
inorder :: BST a -> [a]
inorder Empty             = []
inorder (Node v left right) = inorder left ++ [v] ++ inorder right

-- | Preorder traversal
preorder :: BST a -> [a]
preorder Empty             = []
preorder (Node v left right) = v : preorder left ++ preorder right

-- | Postorder traversal
postorder :: BST a -> [a]
postorder Empty             = []
postorder (Node v left right) = postorder left ++ postorder right ++ [v]

-- | Level order traversal (breadth-first)
levelOrder :: BST a -> [a]
levelOrder tree = go [tree]
  where
    go [] = []
    go (Empty : rest) = go rest
    go (Node v left right : rest) = v : go (rest ++ [left, right])

-- | Create tree from list
fromList :: Ord a => [a] -> BST a
fromList = foldr insert empty . reverse

-- | Convert to sorted list (same as inorder)
toList :: BST a -> [a]
toList = inorder
