module Triangle (rows) where

rows :: Integer -> [[Integer]]
rows n = [row i | i <- [1..n]]
    where
        row :: Integer -> [Integer]
        row j = [ (j-1) `choose` (k-1) | k <- [1..j] ]

        choose :: Integer -> Integer -> Integer
        choose _ 0 = 1
        choose 0 _ = 0
        choose j k = choose (j-1) (k-1) * j `div` k 