module Dominoes (chain) where

import Data.List ( find, delete )
import Data.Tuple ( swap ) 

type Domino = (Int,Int)
type Chain  = [Domino]
type Hand   = [Domino]

chain :: Hand -> Maybe Chain
chain = find equalEnds . allChains []

equalEnds :: Chain -> Bool
equalEnds [] = True
equalEnds c  = fst (head c) == snd (last c)

allChains :: Chain -> Hand -> [Chain]
allChains c [] = [c]
allChains c h  = do
    d <- h
    
    let ds = delete d h

    let putLCheck = null c || snd d == fst (head c) 
    let rotLCheck = null c || fst d == fst (head c)
    let putRCheck = null c || fst d == snd (last c)
    let rotRCheck = null c || snd d == snd (last c)

    let putL = if putLCheck then allChains (d:c)           ds else []
    let rotL = if rotLCheck then allChains (swap d:c)      ds else []
    let putR = if putRCheck then allChains (c ++ [d])      ds else []
    let rotR = if rotRCheck then allChains (c ++ [swap d]) ds else []

    putL ++ rotL ++ putR ++ rotR