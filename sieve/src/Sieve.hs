module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n = fst $ sieve ([], [2..n])
    where
        sieve :: ([Integer],[Integer]) -> ([Integer],[Integer])
        sieve (primes,[]) = (primes,[])
        sieve (primes, candidates) = sieve (primes ++ [nextPrime], filter (not . multiple) candidates)
            where
                nextPrime = head candidates

                multiple :: Integer -> Bool
                multiple k = k `elem` [nextPrime * i | i <- [1..n]]