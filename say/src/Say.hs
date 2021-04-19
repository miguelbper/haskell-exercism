module Say (inEnglish) where

import Data.List.Split ( chunksOf )
import Control.Arrow ( Arrow((&&&)) ) 
import qualified Data.Text as T
inEnglish :: Integer -> Maybe String
inEnglish x
    | x < 0     = Nothing
    | x == 0    = Just "zero"
    | x > 10^12 = Nothing
    | otherwise = Just
                . T.unpack . T.strip . T.pack
                . concat
                . reverse
                . intersperser
                . map inEnglishThousands
                . chunksOfThousands
                . digits
                $ x

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

number :: Integral x => [x] -> x
number xs = sum $ zipWith (*) (reverse xs) [ 10^i | i <- [0..] ]

chunksOfThousands :: [a] -> [[a]]
chunksOfThousands = map reverse . chunksOf 3 . reverse

scaleWords :: [String]
scaleWords = ["", " thousand ", " million ", " billion ", " trillion "]

intersperser :: [String] -> [String]
intersperser = zipWith myConcat scaleWords
    where
        myConcat :: [a] -> [a] -> [a]
        myConcat _ [] = []
        myConcat xs ys = ys ++ xs


inEnglishThousands :: [Integer] -> String
inEnglishThousands xs = hundredString xs' ++ inEnglishHundreds lastTwo
    where
        lastTwo = reverse . take 2 . reverse $ xs'
        xs' = digits . number $ xs
        hundredString xs = if length xs == 3 then baseCase (head xs) ++ " hundred "
                                             else ""

baseCase :: Integer -> String
baseCase x = case x of 
    1  -> "one"
    2  -> "two"
    3  -> "three"
    4  -> "four"
    5  -> "five"
    6  -> "six"
    7  -> "seven"
    8  -> "eight"
    9  -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    19 -> "nineteen"
    _  -> ""

baseCaseTen :: Integer -> String
baseCaseTen x = case x of 
    2  -> "twenty"
    3  -> "thirty"
    4  -> "forty"
    5  -> "fifty"
    6  -> "sixty"
    7  -> "seventy"
    8  -> "eighty"
    9  -> "ninety"

inEnglishHundreds :: [Integer] -> String
inEnglishHundreds (x:y:xs) = if x <= 1 then baseCase (x * 10 + y)
                                       else baseCaseTen x ++ (if y /= 0 then "-" ++ baseCase y else "")
inEnglishHundreds (x:xs)   = baseCase x
inEnglishHundreds []       = ""