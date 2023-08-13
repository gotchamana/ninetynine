{- |
Description: List of prime factors and their multiplicities
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P36".
-}
module Problems.P36 (primeFactorsMultiplicity) where

import Data.List.NonEmpty (group, NonEmpty ((:|)))
import Problems.P35 (primeFactors)

-- | Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.
--
-- === Examples
--
-- >>> primeFactorsMultiplicity 315
-- [(3,2),(5,1),(7,1)]
primeFactorsMultiplicity :: Integral a => a -> [(a, a)]
primeFactorsMultiplicity = map (\xs@(x :| _) -> (x, fromIntegral $ length xs)) . group . primeFactors
