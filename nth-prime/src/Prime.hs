module Prime (nth) where

primes :: Int -> [Int]
primes n
    | n <= 0 = []
    | n == 1 = [2]
    | n >= 2 = primesBefore ++ [nthPrime]
    where
        primesBefore  = primes (n - 1)
        primePrevious = last primesBefore
        nthPrime      = head [ x | x <- [(primePrevious + 1)..], all (\p -> x `mod` p /= 0) primesBefore ]

nth :: Int -> Maybe Integer
nth n
    | n <= 0 = Nothing 
    | n >= 1 = Just (fromIntegral $ last (primes n))