module Minesweeper (annotate) where

import Data.Char ( intToDigit )
import Data.List.Split ( chunksOf )
import Control.Applicative ( Applicative(liftA2) )

type Index       = (Int, Int)
type Mine        = Maybe Int
type MinePos     = (Index, Mine)
type Minesweeper = [MinePos]

intFromChar :: Char -> Maybe Int
intFromChar '*' = Nothing
intFromChar _   = Just 0

charFromInt :: Maybe Int -> Char
charFromInt Nothing  = '*'
charFromInt (Just 0) = ' '
charFromInt (Just x) = intToDigit x

indexList :: Int -> Int -> [Index]
indexList y x = [ (i, j) | i <- [0..(x-1)], j <- [0..(y-1)] ]

mineFromStr :: [String] -> Minesweeper
mineFromStr xs = zip (indexList x y) (map intFromChar . concat $ xs)
    where
        x = length . head $ xs
        y = length xs

strFromMine :: Minesweeper -> [String]
strFromMine xs = chunksOf n . map (charFromInt . snd) $ xs
    where
        n = (+ 1) . maximum . map (snd . fst) $ xs

minePosSum :: MinePos -> MinePos -> MinePos
minePosSum (i,m) (j,n) = (i, liftA2 (+) m n)

minesweeperSum :: Minesweeper -> Minesweeper -> Minesweeper
minesweeperSum = zipWith minePosSum

surroundOnes :: Int -> Int -> MinePos -> Minesweeper
surroundOnes d1 d2 ((i,j), x)  = [ ((k, l), f x i j k l) | k <- [0..(d2-1)], l <- [0..(d1-1)] ]
    where
        isAround :: Int -> Int -> Int -> Int -> Bool
        isAround i j k l = (k - i) `elem` [-1,0,1] && (l - j) `elem` [-1,0,1]

        f :: Maybe Int -> Int -> Int -> Int -> Int -> Maybe Int
        f (Just x) _ _ _ _ = Just 0
        f Nothing  i j k l = if isAround i j k l then Just 1
                                                 else Just 0

fill :: MinePos -> Minesweeper -> Minesweeper
fill p m = minesweeperSum (surroundOnes d1 d2 p) m
    where
        d1 = (+ 1) . maximum . map (snd . fst) $ m
        d2 = (+ 1) . maximum . map (fst . fst) $ m

annotateMine :: Minesweeper -> Minesweeper
annotateMine mine = foldr fill mine mine

annotate :: [String] -> [String]
annotate []   = []
annotate [""] = [""]
annotate xs   = strFromMine . annotateMine . mineFromStr $ xs