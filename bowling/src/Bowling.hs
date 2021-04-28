module Bowling (score, BowlingError(..)) where

import Data.Tuple ( swap )
import Control.Monad ( liftM2 )
import Data.List (intercalate)
import Data.List.Split (splitOn)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

type Throw = Int
type Frame = (Throw, Throw)
type Game  = [Frame]
type Score = Int

isOpen :: Frame -> Bool
isOpen = (< 10) . uncurry (+)

isSpare :: Frame -> Bool
isSpare = liftM2 (&&) ( (== 10) . uncurry (+) ) ( not . isStrike )

isStrike :: Frame -> Bool
isStrike = (10 == ) . fst

gameFromThrows :: [Throw] -> Game
gameFromThrows = gameFromThrowsHelper . insertZeros
    where
        gameFromThrowsHelper :: [Throw] -> Game
        gameFromThrowsHelper []       = []
        gameFromThrowsHelper (x:y:xs) = (x,y) : gameFromThrowsHelper xs
        gameFromThrowsHelper (x:xs)   = [] 

        insertZeros :: [Throw] -> [Throw]
        insertZeros = map snd . insertZerosHelper . zip [0,1..]
            where
                addOneToInd :: [(Int, a)] -> [(Int, a)]
                addOneToInd = map $ swap . fmap (+1) . swap
        
                insertZerosHelper :: [(Int,Throw)] -> [(Int,Throw)]
                insertZerosHelper [] = []
                insertZerosHelper ((i,x) : xs)
                    | x == 10 && even i = (:) (i,x) 
                                        . (:) (i+1,0) 
                                        . insertZerosHelper
                                        . addOneToInd 
                                        $ xs
                    | otherwise         = (i,x) : insertZerosHelper xs

isValidInput :: [Throw] -> Bool
isValidInput = liftM2 (&&) throwsInRange correctNThrows
    where
        throwsInRange = all (`elem` [0..10])

        correctNThrows :: [Int] -> Bool
        correctNThrows xs = True

isIncomplete :: Game -> Bool
isIncomplete xs = (length xs < 10)
               || (isStrike (xs !! 9) || isSpare (xs !! 9)  )

score :: [Throw] -> Either BowlingError Score
score rolls = error "You need to implement this function."