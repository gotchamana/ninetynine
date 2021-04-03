{- |
Description: Extract a slice from a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P18".
-}
module Problems.P18 (slice) where

import qualified Solutions.P18 as Solution

-- | Extract a slice from a list.
--
-- Given two indices, @i@ and @k@, the slice is the list containing the elements
-- between the @i@'th and @k@'th element of the original list, with both limits included.
-- Start counting the elements with 1.
--
-- === Examples
--
-- >>> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice = Solution.slice
