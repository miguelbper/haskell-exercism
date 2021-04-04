module OCR (convert) where

import Data.List ( transpose, intercalate )
import Data.List.Split ( chunksOf )

convert' :: [String] -> Char
convert' xs = case xs of
                   ["   ","  |","  |","   "] -> '1'
                   [" _ "," _|","|_ ","   "] -> '2'
                   [" _ "," _|"," _|","   "] -> '3'
                   ["   ","|_|","  |","   "] -> '4'
                   [" _ ","|_ "," _|","   "] -> '5'
                   [" _ ","|_ ","|_|","   "] -> '6'
                   [" _ ","  |","  |","   "] -> '7'
                   [" _ ","|_|","|_|","   "] -> '8'
                   [" _ ","|_|"," _|","   "] -> '9'
                   [" _ ","| |","|_|","   "] -> '0'
                   _                         -> '?'

convert :: String -> String
convert = intercalate ","
        . map (map convert'
             . transpose
             . map (chunksOf 3))
        . chunksOf 4
        . lines