{- |
Description: Indexed element in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P03".
-}
module Problems.P03 (elementAt) where

-- | Find the @k@th element of a list.
-- The first element in the list is number 1.
--
-- === Examples
--
-- >>> elementAt [1,2,3] 2
-- Just 2
--
-- >>> elementAt "haskell" 5
-- Just 'e'
--
-- >>> elementAt [1,2] 3
-- Nothing
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x : xs) n
    | n < 1 = Nothing
    | n == 1 = Just x
    | otherwise = elementAt xs (n - 1)
