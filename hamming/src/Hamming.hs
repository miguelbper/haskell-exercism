module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise              = Just $ length [ x | x <- zip xs ys, uncurry (/=) x ]