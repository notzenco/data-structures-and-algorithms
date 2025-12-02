{-|
Module      : DataStructures.HashTable
Description : Hash table implementation using Data.HashMap
Stability   : stable

A hash table wrapper around Data.HashMap.Strict.
Time: O(1) average for all operations
Space: O(n)
-}
module DataStructures.HashTable
  ( HashTable
  , empty
  , isEmpty
  , put
  , get
  , remove
  , contains
  , keys
  , values
  , size
  , fromList
  , toList
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)

-- | A hash table backed by HashMap
newtype HashTable k v = HashTable (HM.HashMap k v)
  deriving (Show, Eq)

-- | Create an empty hash table
empty :: HashTable k v
empty = HashTable HM.empty

-- | Check if hash table is empty
isEmpty :: HashTable k v -> Bool
isEmpty (HashTable m) = HM.null m

-- | Insert or update a key-value pair
put :: (Eq k, Hashable k) => k -> v -> HashTable k v -> HashTable k v
put k v (HashTable m) = HashTable (HM.insert k v m)

-- | Get value by key
get :: (Eq k, Hashable k) => k -> HashTable k v -> Maybe v
get k (HashTable m) = HM.lookup k m

-- | Remove a key-value pair
remove :: (Eq k, Hashable k) => k -> HashTable k v -> (Maybe v, HashTable k v)
remove k (HashTable m) = (HM.lookup k m, HashTable (HM.delete k m))

-- | Check if key exists
contains :: (Eq k, Hashable k) => k -> HashTable k v -> Bool
contains k (HashTable m) = HM.member k m

-- | Get all keys
keys :: HashTable k v -> [k]
keys (HashTable m) = HM.keys m

-- | Get all values
values :: HashTable k v -> [v]
values (HashTable m) = HM.elems m

-- | Get the number of key-value pairs
size :: HashTable k v -> Int
size (HashTable m) = HM.size m

-- | Create from list of key-value pairs
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashTable k v
fromList = HashTable . HM.fromList

-- | Convert to list of key-value pairs
toList :: HashTable k v -> [(k, v)]
toList (HashTable m) = HM.toList m
