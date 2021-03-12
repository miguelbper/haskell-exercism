module Acronym (abbreviate) where

import Data.Text as T (Text, dropWhile, takeWhile, append, head, tail, pack, empty, filter, toUpper)
import Data.Char as C (toUpper, isUpper)

abbreviate :: Text -> Text
abbreviate xs
    | xs == empty = empty
    | otherwise   = (abbreviateSW . fw) xs `append` (abbreviate . lw) xs
    where
        isGarbage :: Char -> Bool
        isGarbage c = c == ' ' || c == '_' || c == '-'
        
        fw :: Text -> Text
        fw ys = T.takeWhile (not . isGarbage) (T.dropWhile isGarbage ys)
        
        lw :: Text -> Text
        lw ys = T.dropWhile (not . isGarbage) (T.dropWhile isGarbage ys)
        
        isUpper' :: Text -> Bool
        isUpper' xs = xs == T.toUpper xs
        
        abbreviateSW :: Text -> Text
        abbreviateSW xs
            | isUpper' xs = pack [(C.toUpper . T.head) xs] 
            | otherwise   = pack [(C.toUpper . T.head) xs] `append` T.filter isUpper (T.tail xs)