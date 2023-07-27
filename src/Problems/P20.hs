{- |
Description: Remove element from a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P20".
-}
module Problems.P20 (removeAt) where

-- | Remove the @k@th element from a list.
-- Return the element removed and the residue list.
--
-- === Examples
--
-- >>> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Empty list"
removeAt 1 (x : xs) = (x, xs)
removeAt n (x : xs) =
    let (y, ys) = removeAt (n - 1) xs
     in if n <= 0
            then error $ "Invalid index: " <> show n
            else (y, x : ys)
