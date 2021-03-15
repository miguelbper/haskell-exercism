module Raindrops (convert) where

convert :: Int -> String
convert n
    | null factors = convert' n
    | otherwise    = concatMap convert' factors
    where
        factors = [ x | x <- [3, 5, 7], x `isFactor` n]

        convert' :: Int -> String
        convert' 3 = "Pling"
        convert' 5 = "Plang"
        convert' 7 = "Plong"
        convert' x = show x

        isFactor :: Int -> Int -> Bool
        isFactor x m = m `mod` x == 0
    