module Change (findFewestCoins) where

import Control.Monad ( liftM2 )

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = 
    if null newCoins 
        then Nothing
        else fmap (change ++ ) (findFewestCoins newTarget coins)
    where
        newCoins  = filter (<= target) coins
        bigCoin   = maximum . filter (< target) $ coins
        nBigCoin  = target `div` bigCoin
        newTarget = target `mod` bigCoin
