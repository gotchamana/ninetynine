{- |
Description: Drop elements in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P16".
-}
module Problems.P16 (dropEvery) where

-- | Drop every @n@th element from a list.
--
-- === Examples
--
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery xs n
    | n <= 0 = xs
    | n == 1 = []
    | otherwise = go xs n
  where
    go [] _ = []
    go (x' : xs') n' = if n' == 1 then go xs' n else x' : go xs' (n' - 1)
