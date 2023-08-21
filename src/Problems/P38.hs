{- |
Description: Highly totient numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P38".
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Problems.P38 (highlyTotientNumbers) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as Map
import Problems.P37 (totient')
import Safe (maximumByMay)

{- |
It is possible for more than one number \(x\) to have the same totient number \(\phi(x)\).
For example, \(\phi(77) = \phi(93) = 60\).

A highly totient number \(n\) has the most such \(x\)
among the totient numbers less than or equal to \(n\).
I.e., \(n\) is a highly totient number
if \(|\{x \,|\, \phi(x)=n \}| > |\{x \,|\, \phi(x)=m \}|\) for \(m < n\).

Construct the list of highly totient numbers.

=== Examples

>>> take 10 highlyTotientNumbers
[1,2,4,8,12,24,48,72,144,240]

=== __Hint__

Given a totient number \(n=\phi(x)\), find an upper bound for \(x\),
which will bound the set of numbers from which to search for solutions.
Compare the prime factorizations between \(\phi(x)\) and \(x\),
and devise a modification of the former so that it must be larger than the latter.
-}
highlyTotientNumbers :: Integral a => [a]
highlyTotientNumbers = go S{nFreq = Map.singleton 1 1, x = 2, preBound = 1, maxNFreq = Nothing}
  where
    go state@S{..} =
        let lowerBound = ceiling @Double @Int (sqrt (fromIntegral x / 2))
            (lowerMap, pivot, largerMap) = Map.splitLookup lowerBound nFreq
            usedMap =
                if preBound < lowerBound
                    then maybe largerMap (\f -> Map.insert lowerBound f largerMap) pivot
                    else nFreq
            nFreq' = Map.insertWith (+) (totient' x) 1 usedMap
            maxEntry = do
                guard $ preBound < lowerBound

                entry@(n, f) <- maximumByMay (compare `on` snd) $ Map.toList lowerMap

                case maxNFreq of
                    Just (n', f') -> if n > n' && f > f' then return entry else Nothing
                    Nothing -> return entry
            newState = state{nFreq = nFreq', x = x + 1, preBound = lowerBound, maxNFreq = maxEntry <|> maxNFreq}
         in maybe (go newState) (\(n, _) -> fromIntegral n : go newState) maxEntry

data S = S
    { nFreq :: IntMap Int
    , x :: Int
    , preBound :: Int
    , maxNFreq :: Maybe (Int, Int)
    }
