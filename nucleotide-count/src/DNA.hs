module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
    | isValid   = Right nuclMap
    | otherwise = Left $ error "that string is invalid"
    where
        countAll = length xs
        countOfA = length (filter (== 'A') xs)
        countOfC = length (filter (== 'C') xs)
        countOfG = length (filter (== 'G') xs)
        countOfT = length (filter (== 'T') xs)

        isValid = countAll == countOfA + countOfC + countOfG + countOfT

        nuclMap = Map.fromList [(A, countOfA), (C, countOfC), (G, countOfG), (T, countOfT)]