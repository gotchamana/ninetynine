{- |
Description: Pack duplicates in a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P09".
-}
module Problems.P09 (pack) where

import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE

-- | Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements, they should be placed in separate sublists.
--
-- === Examples
--
-- >>> pack "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack = map NE.toList . foldr f []
  where
    f e [] = [e :| []]
    f e acc@(xs@(x :| _) : xss) = if e == x then (e <| xs) : xss else (e :| []) : acc
