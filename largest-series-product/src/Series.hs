module Series (Error(..), largestProduct) where

import Data.Char ( digitToInt )

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
    | invalidSpan                = Left InvalidSpan
    | not . null $ invalidDigits = Left (InvalidDigit (head invalidDigits))
    | otherwise                  = Right $ toInteger . maximum . map product . substrings size . map digitToInt $ digits
    where
        invalidSpan = size < 0 || length digits < size

        invalidDigits = filter (not . (`elem` ['0'..'9'])) digits

        substrings :: Int -> [a] -> [[a]]
        substrings n xs
            | length xs <= n = [xs]
            | otherwise      = take n xs : substrings n (drop 1 xs)
