module PigLatin (translate) where

import Data.Text as T ( Text(..), replace, pack, takeWhile, dropWhile, append, cons, head, take, tail, null, empty )

convert :: Text -> Text
convert = replace (pack "xr") (pack "#")
        . replace (pack "yt") (pack "$")
        . replace (pack "qu") (pack "%")

unconvert :: Text -> Text
unconvert = replace (pack "#") (pack "xr")
          . replace (pack "$") (pack "yt")
          . replace (pack "%") (pack "qu")

vowelInit :: [Char]
vowelInit = ['a','e','i','o','u','#','$']

vowelMid :: [Char]
vowelMid = ['a','e','i','o','u','%']

cluster :: Text -> Text
cluster text 
    | T.null text = T.empty 
    | otherwise   = if T.head text `elem` vowelInit 
                        then empty 
                        else T.takeWhile (`notElem` vowelMid) text

rest :: Text -> Text
rest text 
    | T.null text = T.empty 
    | otherwise   = if T.head text `elem` vowelInit 
                        then text 
                        else T.dropWhile (`notElem` vowelMid) text

translate :: Text -> Text
translate xs = unconvert ((rest . convert $ xs) `append` (cluster . convert $ xs)) `append` pack "ay"
