module Luhn (isValid) where

import Data.Char ( digitToInt, isDigit )
import Control.Monad ( liftM2 )

isValid :: String -> Bool
isValid = liftM2 (&&) bigLength luhn . normalize
    where
        normalize :: String -> [Int]
        normalize = reverse . map digitToInt . filter isDigit

        bigLength :: [a] -> Bool
        bigLength = (1 <) . length

        luhn :: [Int] -> Bool
        luhn = (== 0) . flip mod 10 . luhnSum
            where
                luhnSum :: [Int] -> Int
                luhnSum [ ]      = 0
                luhnSum [x]      = x
                luhnSum (x:y:xs) = x
                                 + (if 2 * y < 9 then 2 * y else 2 * y - 9)
                                 + luhnSum xs