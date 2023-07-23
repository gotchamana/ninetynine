{- |
Description: Eliminate duplicate elements in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P08".
-}
module Problems.P08 (compress) where

-- | Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements,
-- they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
--
-- === Examples
--
-- >>> compress "aaaabccaadeeee"
-- "abcade"
compress :: Eq a => [a] -> [a]
compress = foldr f []
  where
    f e [] = [e]
    f e acc@(x : _) = if e == x then acc else e : acc
