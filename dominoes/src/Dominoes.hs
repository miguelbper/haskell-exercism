module Dominoes (chain) where

import Data.List ( find, permutations )
import Data.Tuple ( swap ) 

type Domino = (Int,Int)
type Chain  = [Domino]
type Hand   = [Domino]

chain :: Hand -> Maybe Chain
chain = find equalEnds . concatMap (allChains []) . permutations

equalEnds :: Chain -> Bool
equalEnds [] = True
equalEnds c  = fst (head c) == snd (last c)

allChains :: Chain -> Hand -> [Chain]
allChains c []     = [c]
allChains c (d:ds) = putL ++ rotL ++ putR ++ rotR
    where
        putL = if putLCheck then allChains (d:c)           ds else []
        rotL = if rotLCheck then allChains (swap d:c)      ds else []
        putR = if putRCheck then allChains (c ++ [d])      ds else []
        rotR = if rotRCheck then allChains (c ++ [swap d]) ds else []

        putLCheck = null c || snd d == fst (head c) 
        rotLCheck = null c || fst d == fst (head c)
        putRCheck = null c || fst d == snd (last c)
        rotRCheck = null c || snd d == snd (last c)