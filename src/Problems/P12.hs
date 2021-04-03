{- |
Description: Decode a run-length encoded list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P12".
-}
module Problems.P12 (decodeModified) where

import           Problems.Lists
import qualified Solutions.P12  as Solution

-- | Given a run-length code list generated by 'Problems.P11.encodeModified',
-- construct its uncompressed version.
--
-- === Examples
--
-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: [Encoding a] -> [a]
decodeModified = Solution.decodeModified
