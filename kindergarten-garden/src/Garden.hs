module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Char

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Student = String

data GardenSq = GardenSq { st :: Student,
                           p1 :: Plant,
                           p2 :: Plant,
                           p3 :: Plant,
                           p4 :: Plant }

type Garden = [GardenSq]

garden :: [Student] -> String -> Garden
garden students plants = garden' students (row1 plants) (row2 plants)
    where
        row1 :: String -> String
        row1 pl = filter (/= '\n') . takeWhile (/= '\n') $ plants

        row2 :: String -> String
        row2 pl = filter (/= '\n') . dropWhile (/= '\n') $ plants

        plant :: Char -> Plant
        plant c
            | cUpper == 'C' = Clover
            | cUpper == 'G' = Grass
            | cUpper == 'R' = Radishes
            | cUpper == 'V' = Violets
            where cUpper = toUpper c

        garden' :: [Student] -> String -> String -> Garden
        garden' [] _ _ = []
        garden' _ [] _ = []
        garden' _ _ [] = []
        garden' (x:xs) (p:q:pl) (r:s:rl) = GardenSq x (plant p) (plant q) (plant r) (plant s) : garden' xs pl rl

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants student garden = 
    let gardensq = head . filter ((== student) . st) $ garden
    in [p1 gardensq, p2 gardensq, p3 gardensq, p4 gardensq]

