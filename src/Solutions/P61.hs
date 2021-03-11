{- |
Description: `leaves`

Some solutions to "Problems.P61" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P61 (leaves,internals) where

import           Problems.BinaryTrees

-- | Collect the leaves of a binary tree in a list.  A leaf is a node with no successors.
leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r)         = leaves l ++ leaves r

-- | Collect the internal nodes of a binary tree in a list.
--
-- An internal node of a binary tree has either one or two non-empty successors.
internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r)         = internals l ++ [x] ++ internals r
