{-# LANGUAGE LambdaCase #-}

{- |
Description: Sorting a list of lists according to length of sublists
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P28".
-}
module Problems.P28 (lsort, lfsort) where

import Data.Foldable (toList)
import Data.IntMap.Strict qualified as Map
import Data.List (foldl', sortOn)
import Data.Sequence ((<|))
import Data.Sequence qualified as Seq

-- | We suppose that a list contains elements that are lists themselves.
-- Write a function to sort the elements of this list according to their length,
-- i.e., short lists first and longer lists later.
--
-- === Examples
--
-- >>> lsort ["xxx","xx","xxx","xx","xxxx","xx","x"]
-- ["x","xx","xx","xx","xxx","xxx","xxxx"]
lsort :: [[a]] -> [[a]]
lsort = map snd . sortOn fst . map (\xs -> (length xs, xs))

-- | Again, we suppose that a list contains elements that are lists themselves.
-- But this time, write a function to sort the elements of this list according to their length frequency,
-- i.e., lists with rare lengths are placed first, others with a more frequent length come later.
--
-- === Examples
--
-- >>> lfsort ["xxx", "xx", "xxx", "xx", "xxxx", "xx"]
-- ["xxxx","xxx","xxx","xx","xx","xx"]
lfsort :: [[a]] -> [[a]]
lfsort xss =
    let lengthMap = foldl' (\m xs -> Map.alter (f xs) (length xs) m) Map.empty xss
     in toList . mconcat . sortOn Seq.length . Map.elems $ lengthMap
  where
    f xs (Just s) = Just (xs <| s)
    f xs Nothing = Just (Seq.singleton xs)
