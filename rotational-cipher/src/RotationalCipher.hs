module RotationalCipher (rotate) where

import Data.Char

rotate :: Int -> String -> String
rotate n = map cipher
    where
        alphalen = length ['a'..'z']

        cipher :: Char -> Char
        cipher c
            | isUpper c = toEnum . shift (fromEnum 'A') . fromEnum $ c 
            | isLower c = toEnum . shift (fromEnum 'a') . fromEnum $ c
            | otherwise = c

        shift :: Int -> Int -> Int
        shift m x = ((x - m + n) `mod` alphalen) + m
