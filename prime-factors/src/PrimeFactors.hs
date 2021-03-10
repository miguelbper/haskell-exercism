module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` head factors)
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. floor . sqrt $ fromIntegral n]