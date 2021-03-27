module Atbash (decode, encode) where

import Data.Maybe ( fromMaybe )
import Data.List ( elemIndex ) 
import Data.Char ( toLower ) 
import Data.List.Split ( chunksOf )

alphabet    = ['a'..'z']
reversebet  = reverse alphabet
punctuation = [' ', '.', ',', '!']

atbash :: Char -> Char
atbash x
    | toLower x `elem` alphabet = reversebet !! fromMaybe 0 (elemIndex (toLower x) alphabet)
    | otherwise                 = x

decode :: String -> String
decode = map atbash . filter (not . (`elem` punctuation))

encode :: String -> String
encode = unwords . chunksOf 5 . decode
