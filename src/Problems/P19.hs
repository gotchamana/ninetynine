{- |
Description: 'rotate'

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P19".
-}
module Problems.P19 (rotate) where

import qualified Solutions.P19 as Solution

-- | Rotate a list @n@ places to the left.
--
-- === Examples
--
-- >>> rotate "abcdefgh" 3
-- "defghabc"
--
-- >>> rotate "abcdefgh" (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate = Solution.rotate
