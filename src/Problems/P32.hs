{- |
Description: Greatest common divisor
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P32".
-}
module Problems.P32 (myGCD) where

import GHC.Stack (HasCallStack)

-- | Determine the greatest common divisor of two positive integer numbers.
-- Use [Euclid's algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm).
--
-- === Examples
--
-- >>> myGCD 36 63
-- 9
--
-- >>> myGCD 125 81
-- 1
--
-- >>> myGCD 221 559
-- 13
myGCD :: (HasCallStack, Integral a) => a -> a -> a
myGCD a b =
    if a == 0 || b == 0
        then error "Invalid argument"
        else
            let a' = abs a
                b' = abs b
                a'' = max a' b'
                b'' = min a' b'
                r = a'' `mod` b''
             in if r == 0 then b'' else myGCD b'' r
