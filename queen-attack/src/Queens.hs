module Queens (boardString, canAttack) where

import Data.List ( intersperse )

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = zipWith charOr (insertOne 'W' white) (insertOne 'B' black)
    where
        charOr :: Char -> Char -> Char
        charOr '_' '_' = '_'
        charOr c   '_' = c
        charOr _   d   = d
        
        insertOne :: Char -> Maybe (Int, Int) -> String
        insertOne c Nothing  = unlines . replicate 8 . intersperse ' ' . replicate 8 $ '_'
        insertOne c (Just p) = unlines (firstDashes ++ [line] ++ lastDashes)
            where
                x = fst p
                y = snd p
        
                firstDashes = replicate x       . intersperse ' ' . replicate 8 $ '_'
                lastDashes  = replicate (8-x-1) . intersperse ' ' . replicate 8 $ '_'
        
                line = intersperse ' ' (replicate y '_' ++ [c] ++ replicate (8-y-1) '_')

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB = queenB `elem` targets
    where
        targets = hor ++ ver ++ dg1 ++ dg2

        x = fst queenA
        y = snd queenA

        hor = [(x, y) | x <- [0..7] ] 
        ver = [(x, y) | y <- [0..7] ]
        dg1 = [(x + i, y + i) | i <- [(-7)..7] ]
        dg2 = [(x + i, y - i) | i <- [(-7)..7] ]
