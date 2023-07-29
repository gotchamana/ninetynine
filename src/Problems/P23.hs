{- |
Description: Select random elements from a list
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P23".
-}
module Problems.P23 (randomSelect, shuffle) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (MonadState (get, put), MonadTrans (lift), execStateT)
import Data.Array.MArray (MArray (getBounds), newListArray, readArray, writeArray, getElems)
import Data.Array.ST (STArray)
import Data.Ix (Ix)
import System.Random (Random (randomR), RandomGen)

-- | Extract a given number of randomly selected elements from a list.
--
-- Also return a new random number generator so that callers
-- can avoid reusing a sequence of random numbers.
--
-- === Examples
--
-- >>> fst $ randomSelect "abcdefgh" 3 $ mkStdGen 111
-- "chd"
--
-- >>> take 5 $ unfoldr (Just . randomSelect [1..100] 3) $ mkStdGen 111
-- [[11,19,76],[63,49,10],[75,42,12],[20,48,78],[40,94,86]]
--
-- >>> fst . randomSelect "abcdefgh" 3 <$> newStdGen
-- "ebf"
randomSelect :: forall a g. RandomGen g => [a] -> Int -> g -> ([a], g)
randomSelect [] _ g = ([], g)
randomSelect xs n g =
    if n <= 0
        then ([], g)
        else runST $ do
            arr <- newListArray (1, length xs) xs :: ST s (STArray s Int a)
            g' <- shuffle arr g
            ys <- take n <$> getElems arr

            return (ys, g')

shuffle :: (RandomGen g, MArray a e m) => a Int e -> g -> m g
shuffle arr = execStateT $ do
    (low, high) <- lift $ getBounds arr

    forM_ [low .. high - 1] $ \i -> do
        g <- get

        let (j, g') = randomR (i, high) g
        put g'

        lift $ swap arr i j

swap :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swap arr i j = do
    e1 <- readArray arr i
    e2 <- readArray arr j

    writeArray arr i e2
    writeArray arr j e1
