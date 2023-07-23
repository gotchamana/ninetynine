{- |
Description: Replicate elements of a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P15".
-}
module Problems.P15 (repli) where

-- | Replicate the elements of a list a given number of times.
--
-- === Examples
--
-- >>> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs
