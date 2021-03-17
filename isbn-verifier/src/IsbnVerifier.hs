module IsbnVerifier (isbn) where

import Data.Char ( digitToInt )

isbn :: String -> Bool
isbn xs = (0 == sum' `mod` 11) && valid
    where
        sum' :: Int
        sum' = sum (zipWith (*) [1..10] str)

        str :: [Int]
        str = map read' . reverse . filter (/= '-') $ xs

        read' :: Char -> Int
        read' c
            | c `elem` ['0'..'9'] = digitToInt c
            | c == 'X'            = 10
            | otherwise           = -1

        valid :: Bool
        valid 
            | null str  = False
            | otherwise = head str `elem` [0..10] && all (`elem` [0..9]) (tail str) && length str == 10