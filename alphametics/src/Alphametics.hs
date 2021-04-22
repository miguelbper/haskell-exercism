module Alphametics (solve) where

import Data.Map ( Map, toList, fromList )
import Data.List ( permutations, sort )
import Data.Char ( isAlpha )

type Key = Map Char Int

possibleKeys :: String -> [Key]
possibleKeys xs = if length al > 10 then [] else [ fromList (zip al nn) | nn <- nss ]
    where
        al  = sort $ filter isAlpha xs
        ns  = [0..9]
        nss = permutations ns

testKey :: String -> Key -> Bool
testKey xs k = False

headUnique :: [a] -> Maybe a
headUnique []  = Nothing
headUnique [x] = Just x
headUnique _   = Nothing

solve :: String -> Maybe [(Char, Int)]
solve puzzle = headUnique [ toList key | key <- possibleKeys puzzle, testKey puzzle key ]
