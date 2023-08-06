{- |
Description: Combinations
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P26".
-}
module Problems.P26 (combinations) where


-- $setup
-- >>> import Data.List (sort)

{- |
Generate the combinations of \(k\) distinct objects chosen from the \(n\) elements of a list.

In how many ways can a committee of 3 be chosen from a group of 12 people?
We all know that there are \({12 \choose 3} = 220\) possibilities,
where \(n \choose k\) denotes the binomial coefficient.
For pure mathematicians, this result may be great.
But we want to really generate all the possibilities in a list.

=== Examples

>>> length $ combinations 3 [1..12]
220

>>> sort $ combinations 3 "abcdef"
["abc","abd","abe",...]
-}
combinations :: Int -> [a] -> [[a]]
combinations n xs = go n xs (length xs)
  where
    go n' ys len
        | n' <= 0 = [[]]
        | n' > len = []
        | otherwise =
            concatMap
                (\(x, xs') -> (x :) <$> go (n' - 1) xs' (len - 1))
                (take (len - n' + 1) . tailsWithElem $ ys)

tailsWithElem :: [a] -> [(a, [a])]
tailsWithElem = fst . foldr (\e ~(xs, ys) -> ((e, ys) : xs, e : ys)) ([], [])
