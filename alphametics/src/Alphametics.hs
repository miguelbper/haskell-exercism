module Alphametics (solve) where

import Data.Char ( intToDigit, isAlpha )
import Data.Map ( Map, toList, fromList, (!) )
import Data.List ( permutations, sort, nub, find )
import Data.List.Split ( splitOn )
import Control.Monad ( liftM2 )
import Control.Arrow ( Arrow((&&&)) )

type Key = Map Char Int

-- this solution works but is it very slow

solve :: String -> Maybe [(Char, Int)]
solve = fmap toList 
      . uncurry ($) 
      . (find . testKey &&& possibleKeys ) 
      . format
    
format :: String -> String
format = filter (liftM2 (||) isAlpha (`elem` ['=','+']))

possibleKeys :: String -> [Key]
possibleKeys xs = [ fromList (zip al nn) | nn <- nss ]
    where
        al  = nub . sort . filter isAlpha $ xs
        nss = permutations [0..9]

testKey :: String -> Key -> Bool
testKey = curry $ liftM2 (&&) correctFormula noZeros . splitPuzzle
    where
        splitPuzzle :: (String,Key) -> [[String]]
        splitPuzzle = map (splitOn "+") . splitOn "==" . uncurry convert

        correctFormula :: [[String]] -> Bool
        correctFormula = allTheSame . map ( sum . map read )

        noZeros :: [[String]] -> Bool
        noZeros = all ( (/= '0') . head) . concat

        allTheSame :: (Eq a) => [a] -> Bool
        allTheSame = and . (zipWith (==) <*> tail)

        convert :: String -> Key -> String
        convert xs k = foldr f [] xs
            where
                f :: Char -> String -> String
                f c = if isAlpha c then (:) . intToDigit . ( k ! ) $ c
                                   else (:) c