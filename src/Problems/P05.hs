{- |
Description: Reverse a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P05".
-}
module Problems.P05 (myReverse) where

-- | Reverse a list.
--
-- === Examples
--
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
--
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]
myReverse :: [a] -> [a]
myReverse = go []
  where
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs
