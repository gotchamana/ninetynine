{- |
Description: 'decodeModified'

Some solutions to "Problems.P12" of Ninety-Nine Haskell "Problems".
-}
module Solutions.P12 (decodeModified, decodeModified') where

import           Problems.Lists

-- | Decode a run-length encoded list.
--
-- Given a run-length code list generated by 'Problems.P11.encodeModified',
-- construct its uncompressed version.
--
-- Duplicates each element as appropriate and concatenates them together.
decodeModified :: [Encoding a] -> [a]
decodeModified []                  = []
decodeModified (Single x : xs)     = x : decodeModified xs
decodeModified (Multiple n x : xs) = replicate n x ++ decodeModified xs

-- | Decode a run-length encoded list.
--
-- Given a run-length code list generated by 'Problems.P11.encodeModified',
-- construct its uncompressed version.
--
-- Prepend each element as many times as required, starting from the last element
-- prepended to an empty list, and all the way to the first element.
decodeModified' :: [Encoding a] -> [a]
decodeModified' l = accumulate' l []

accumulate' :: [Encoding a] -> [a] -> [a]
accumulate' [] xs     = xs
accumulate' (x:xs) ys = duplicate' x $ accumulate' xs ys

duplicate' :: Encoding a -> [a] -> [a]
duplicate' (Single x) xs     = x : xs
duplicate' (Multiple n x) xs = prepend' n x xs

prepend' :: Int -> a -> [a] -> [a]
prepend' 0 _ xs = xs
prepend' n x xs
  | n > 0 = x : prepend' (n-1) x xs
  | otherwise = undefined
