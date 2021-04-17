module Frequency (frequency) where

import Data.Char ( isAlpha )
import Data.Map ( Map, empty, insertWith, unionsWith )
import Data.Text as T ( Text, foldr, toLower, filter )
import Control.Parallel.Strategies ( using, rdeepseq, parListChunk )

format :: Text -> Text
format = T.filter isAlpha . toLower

addToMap :: (Ord k) => k -> Map k Int -> Map k Int
addToMap = flip (insertWith (+)) 1
 
frequency' :: Text -> Map Char Int
frequency' = T.foldr addToMap empty . format

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = unionsWith (+) ( using (map frequency' texts) strat )
    where
        chunkSize = length texts `div` nWorkers
        strat = parListChunk chunkSize rdeepseq