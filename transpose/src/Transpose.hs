module Transpose (transpose) where

import qualified Data.List as L ( transpose )

transpose :: [String] -> [String]
transpose = L.transpose . pad
    where
        pad :: [String] -> [String]
        pad = foldr padf []

        padf :: String -> [String] -> [String]
        padf xs []       = [xs]
        padf xs (ys:yss) = fill ' ' (length ys) xs : ys : yss
        
        fill :: a -> Int -> [a] -> [a]
        fill a n xs = xs ++ replicate (n - length xs) a