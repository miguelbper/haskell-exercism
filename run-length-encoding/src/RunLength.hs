module RunLength (decode, encode) where

import Data.List ( group )

decode :: String -> String
decode [] = []
decode encodedText = replicate num char ++ decode rest
    where
        nst  = takeWhile (`elem` ['0'..'9']) encodedText
        rst  = dropWhile (`elem` ['0'..'9']) encodedText
        
        num  = if null nst then 1 else read nst
        char = head rst
        rest = tail rst

encode :: String -> String
encode [] = []
encode text = merge lens chars
    where
        grouped = group text
        chars   = map head grouped
        lens    = map length grouped
        
        merge :: [Int] -> [Char] -> String
        merge []     []     = []
        merge (x:xs) (y:ys) = (if x == 1 then "" else show x) ++ y : merge xs ys