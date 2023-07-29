{- |
Description: Random permutation of a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P25".
-}
module Problems.P25 (randomPermute) where

import Control.Monad (liftM2)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, getElems, newListArray)
import Problems.P23 (shuffle)
import System.Random (RandomGen)

-- $setup
-- >>> import Data.List (unfoldr)

-- | Generate a random permutation of the elements of a list.
--
-- === Examples
--
-- >>> fst $ randomPermute [1..10] $ mkStdGen 111
-- [8,4,7,9,3,5,10,2,1,6]
--
-- >>> take 5 $ unfoldr (Just . randomPermute ['a'..'d']) $ mkStdGen 111
-- ["cbad","abdc","abdc","acdb","cdba"]
--
-- >>> fst . randomPermute "abcdef" <$> newStdGen
-- "dcaebf"
randomPermute :: forall a g. RandomGen g => [a] -> g -> ([a], g)
randomPermute xs g = runST $ do
    arr <- newListArray (1, length xs) xs :: ST s (STArray s Int a)
    liftM2 (flip (,)) (shuffle arr g) (getElems arr)
