module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [ x | x <- [1..limit - 1], any (\y -> x `mod` y == 0) (filter (> 0) factors) ]
