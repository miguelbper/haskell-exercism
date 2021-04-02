module CryptoSquare (encode) where

import Data.List ( transpose )
import Data.List.Split ( chunksOf )
import Data.Char ( toLower )
import Control.Arrow ( (&&&) )

encode :: String -> String
encode = unwords
       . transpose
       . uncurry chunksOf
       . (&&&) chunkSize id
       . map toLower
       . removePunctuation
    where
        punctuation = ",;.:-_~^´`!?@%#$& "

        removePunctuation :: String -> String
        removePunctuation = filter (not . flip elem punctuation)

        chunkSize :: String -> Int
        chunkSize xs = c
            where
                l = length . removePunctuation $ xs
                s = floor . sqrt . fromIntegral $ l
                c = s + (if l > s^2 then 1 else 0)