module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = [ (a,b,c) | c <- [1..(floor ((1 / sqrt 2) * fromIntegral n))], 
                                b <- [(ceiling (fromIntegral (n - c) / 2))..(n - c - 1)], 
                                let a = n - b - c, 
                                a^2 + b^2 == c^2 ]
                            