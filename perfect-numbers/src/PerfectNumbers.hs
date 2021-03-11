module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0            = Nothing
    | aliquotSum n == n = Just Perfect
    | aliquotSum n >  n = Just Abundant
    | aliquotSum n <  n = Just Deficient
    | otherwise         = Nothing
    where
        aliquotSum :: Int -> Int
        aliquotSum m = sum [ x | x <- [1..(m - 1)], m `mod` x == 0]

