module Yacht (yacht, Category(..)) where

import Data.List

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht
              deriving (Eq)

yacht :: Category -> [Int] -> Int
yacht cat dice
    | cat == Ones           = (* 1) . length . filter (==1) $ dice  
    | cat == Twos           = (* 2) . length . filter (==2) $ dice
    | cat == Threes         = (* 3) . length . filter (==3) $ dice
    | cat == Fours          = (* 4) . length . filter (==4) $ dice
    | cat == Fives          = (* 5) . length . filter (==5) $ dice
    | cat == Sixes          = (* 6) . length . filter (==6) $ dice
    | cat == LittleStraight = if sort dice == [1,2,3,4,5] then 30       else 0
    | cat == BigStraight    = if sort dice == [2,3,4,5,6] then 30       else 0
    | cat == Yacht          = if allEq dice               then 50       else 0
    | cat == FullHouse      = if fullHouse                then sum dice else 0
    | cat == FourOfAKind    = 
        case () of 
            () | allEq . take 4 . sort $ dice -> sum . take 4 . sort $ dice
            () | allEq . drop 1 . sort $ dice -> sum . drop 1 . sort $ dice
            () | True                         -> 0
    | cat == Choice         = sum dice
    | otherwise             = 0
    where
        fullHouse = (not . allEq $ dice)
                    && (   ((allEq . take 3 . sort $ dice) && (allEq . drop 3 . sort $ dice))
                        || ((allEq . take 2 . sort $ dice) && (allEq . drop 2 . sort $ dice)))

        allEq :: (Eq a) => [a] -> Bool
        allEq lst = all (== head lst) lst