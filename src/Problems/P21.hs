{- |
Description: Insert element into a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P21".
-}
module Problems.P21 (insertAt) where

-- | Insert an element at a given position into a list.
--
-- === Examples
--
-- >>> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x xs@(x' : xs') n =
    if n <= 1
        then x : xs
        else x' : insertAt x xs' (n - 1)
