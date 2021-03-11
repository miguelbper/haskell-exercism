module Series (slices) where

import Data.Char

slices :: Int -> String -> [[Int]]
slices n xs
    | n >  length xs = []
    | n == length xs = [map digitToInt xs]
    | otherwise      = map digitToInt (take n xs) : slices n (tail xs)