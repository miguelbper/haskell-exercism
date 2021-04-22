module Change (findFewestCoins) where

import Safe ( headMay )

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins = fmap headMay . solutions

solutions :: Integer -> [Integer] -> [[Integer]]
solutions _ [] = []
solutions c (x:xs)
    | c <  0    = []
    | c == 0    = [[]]
    | otherwise = solutions c xs ++ map (x : ) ( solutions (c - x) (x : xs) )