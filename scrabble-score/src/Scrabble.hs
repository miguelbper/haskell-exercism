module Scrabble (scoreLetter, scoreWord) where

import Data.Char ( toUpper )

scoreLetter :: Char -> Integer
scoreLetter letter
    | upperLetter `elem` ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'] = 1
    | upperLetter `elem` ['D', 'G']                                         = 2
    | upperLetter `elem` ['B', 'C', 'M', 'P']                               = 3
    | upperLetter `elem` ['F','H','V','W','Y']                              = 4
    | upperLetter `elem` ['K']                                              = 5
    | upperLetter `elem` ['J','X']                                          = 8
    | upperLetter `elem` ['Q','Z']                                          = 10
    | otherwise = 0
    where upperLetter = toUpper letter

scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter
