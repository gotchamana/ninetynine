{- |
Description: Euler's totient function with Euler's product formula
Copyright: Copyright (C) 2021 Yoo Chung
License: GPL-3.0-or-later
Maintainer: dev@chungyc.org

Part of Ninety-Nine Haskell "Problems".  Some solutions are in "Solutions.P37".
-}
module Problems.P37 (totient') where

import Problems.P36 (primeFactorsMultiplicity)

-- | Calculate Euler's totient function \(\phi(m)\).
-- See 'Problems.P34.totient' for the definition of Euler's totient function.
--
-- Implement an improved version based on Euler's product formula.
-- If the prime factors of \(m\) are known, i.e.,
--
-- \[ m = \prod_{i=1}^r {p_i}^{k_i} \]
--
-- where \(p_i\) is prime and \(k_i \geq 1\), then
--
-- \[ \phi(m) = \prod_{i=1}^r {p_i}^{k_i - 1} (p_i - 1) \]
--
-- Compare with the solution for "Problems.P34":
--
-- > $ stack bench --benchmark-arguments="P34/totient P37/totient'"
--
-- === Examples
--
-- >>> totient' 10
-- 4
totient' :: Integral a => a -> a
totient' = product . map (\(p, k) -> p ^ (k - 1) * (p - 1)) . primeFactorsMultiplicity
