{- |
Description: Euler's totient function
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P34".
-}
module Problems.P34 (totient) where

import Problems.P33 (coprime)

-- | Calculate Euler's totient function \(\phi(m)\).
--
-- Euler's so-called totient function \(\phi(m)\) is defined as
-- the number of positive integers \(r\), where \(1 \leq r \leq m\), that are coprime to \(m\).
--
-- For example, with \(m = 10\), \(\{r \,|\, 1 \leq r \leq m, \textrm{coprime to $m$}\} = \{ 1, 3, 7, 9 \}\);
-- thus \(\phi(m) = 4\).  Note the special case of \(\phi(1) = 1\).
--
-- === Examples
--
-- >>> totient 10
-- 4
totient :: Integral a => a -> a
totient n
    | n < 1 = 0
    | n == 1 = 1
    | otherwise = fromIntegral . length . filter (coprime n) $ [1 .. n]
