{- |
Description: Last element of a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P01".
-}
module Problems.P01 (myLast) where

-- | Find the last element of a list.
--
-- === Examples
--
-- >>> myLast [1,2,3,4]
-- Just 4
--
-- >>> myLast ['x','y','z']
-- Just 'z'
--
-- >>> myLast []
-- Nothing
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_ : xs) = myLast xs
