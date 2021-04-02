module BinarySearch (find) where

import Data.Array ( Array, bounds, (!) )

find :: Ord a => Array Int a -> a -> Maybe Int
find array = find' (bounds array) array
    where
        find' :: Ord a => (Int, Int) -> Array Int a -> a -> Maybe Int
        find' (b,e) arr a
            | e >= b && a == xm = Just m
            | e >= b && a <  xm = find' (b, en) arr a
            | e >= b && a >  xm = find' (bn, e) arr a
            | otherwise         = Nothing
            where
                m  = (e + b) `div` 2
                xm = arr ! m
                bn = m + 1
                en = m - 1

