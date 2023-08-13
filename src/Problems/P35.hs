{- |
Description: List of prime factors
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P35".
-}
module Problems.P35 (primeFactors) where

import Control.Monad (guard)
import Control.Monad.State.Strict (MonadState (get, put), execState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Foldable (Foldable (toList), foldlM)
import Data.Sequence qualified as Seq
import Data.Sequence ((|>))

-- | Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors in ascending order.
--
-- === Examples
--
-- >>> primeFactors 315
-- [3,3,5,7]
primeFactors :: forall a. Integral a => a -> [a]
primeFactors n =
    if n <= 1
        then []
        else
            let s = (n, Seq.empty)
                (n', factors) = execState (runMaybeT computation) s
             in toList $ if n' /= 1 then factors |> n' else factors
  where
    sqrtN = truncate @Double @a . sqrt . fromIntegral $ n
    computation = foldlM f () [2 .. sqrtN]
    f _ fac = do
        (n', result) <- get

        guard (n' /= 1)

        let (q, p) = getPower n' fac
        put (q, result <> Seq.replicate (fromIntegral p) fac)

getPower :: Integral a => a -> a -> (a, a)
getPower n 0 = (n, 0)
getPower 0 _ = (0, 0)
getPower n f = go n 0
  where
    go n' !acc =
        let (q, r) = n' `divMod` f
         in if r == 0 then go q (acc + 1) else (n', acc)
