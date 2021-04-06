module PigLatin (translate) where

import qualified Data.Text as T ( replace, pack, unpack )
import Control.Arrow ((&&&))

replace :: String -> String -> String -> String
replace a b c = T.unpack (T.replace (T.pack a) (T.pack b) (T.pack c))

convert :: String -> String
convert = replace "xr" "#"
        . replace "yt" "$"
        . replace "qu" "%"

unconvert :: String -> String
unconvert = replace "#" "xr"
          . replace "$" "yt"
          . replace "%" "qu"

vowelInit :: [Char]
vowelInit = ['a','e','i','o','u','#','$']

vowelMid :: [Char]
vowelMid = ['a','e','i','o','u','y','$']

cluster :: String -> String
cluster [] = []
cluster [x]
    | x `elem` vowelInit = []
    | otherwise          = [x]
cluster (x:y:xs)
    | x `elem` vowelInit = []
    | y == '%'           = x:[y]
    | otherwise          = x : takeWhile (`notElem` vowelMid) (y:xs)

rest :: String -> String
rest [] = []
rest [x]
    | x `elem` vowelInit = [x]
    | otherwise          = []
rest (x:y:xs)
    | x `elem` vowelInit = x:y:xs
    | y == '%'           = xs
    | otherwise          = dropWhile (`notElem` vowelMid) (y:xs)

translate' :: String -> String
translate' = flip (++) "ay"
           . unconvert
           . uncurry (++)
           . (&&&) rest cluster
           . convert

translate :: String -> String
translate = unwords . map translate' . words