{- |
Description: List of prime numbers
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P39".
-}
module Problems.P39 (primesR, primes) where

-- | Given a range of integers by its lower and upper limit, inclusive,
-- construct a list of all prime numbers in that range.
--
-- === Examples
--
-- >>> primesR 10 20
-- [11,13,17,19]
primesR :: Integral a => a -> a -> [a]
primesR lower upper = takeWhile (<= upper) . filter (>= lower) $ primes

-- | Construct the list of all prime numbers.
--
-- === Examples
--
-- >>> take 5 primes
-- [2,3,5,7,11]
primes :: Integral a => [a]
primes = 2 : go primes [3, 5 ..]
  where
    go (x : xs) ys =
        let xs' = iterate (x +) (x * x)
            ys' = next xs' ys
         in head ys' : go xs (tail ys')
    go _ _ = error "Prime number list is infinite"
    next xs'@(x : xs) ys'@(y : ys)
        | y < x = y : next xs' ys
        | y > x = next xs ys'
        | otherwise = next xs ys
    next _ _ = []
