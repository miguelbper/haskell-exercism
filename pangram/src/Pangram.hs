module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = ['a'..'z'] == [ x | x <- ['a'..'z'], x `elem` map toLower text]
