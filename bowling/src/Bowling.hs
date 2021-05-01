module Bowling (score, BowlingError(..)) where

import Control.Monad ( liftM2 )

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

-- data types
type Score    = Int 
type FrameNr  = Int 
type Index    = Int
type ThrowVal = Int 
type Frame    = (FrameNr, Index, ThrowVal, FrameSc)
type Throws   = [ThrowVal]
type Game     = [Frame]
data FrameSc  = Error | First | Fill | Open | Spare | Strike deriving (Eq, Show)

-- main functions
score :: Throws -> Either BowlingError Score
score ts = if cl then sc else Left IncompleteGame
    where
        gs = gameFromThrows ts
        cl = complete gs
        sc = score' gs

score' :: Game -> Either BowlingError Score
score' []     = Right 0
score' (x:xs) = if condition then result else invRoll
    where
        y = xs !! 0
        z = xs !! 1

        (_  , xii, xtv, xfs) = x
        (_  , _  , ytv, _  ) = y
        (_  , _  , ztv, _  ) = z

        condition
            | xfs == Error  = False
            | xfs == Strike = length xs >= 2
            | xfs == Spare  = length xs >= 1
            | otherwise     = True

        scr
            | xfs == Strike = xtv + ytv + ztv
            | xfs == Spare  = xtv + ytv
            | otherwise     = xtv

        invRoll = Left (InvalidRoll xii xtv)

        result = liftM2 (+) (Right scr) (score' xs)

complete :: Game -> Bool
complete = const True

-- convert
gameFromThrows :: Throws -> Game
gameFromThrows = foldl addThrow []

addThrow :: Game -> ThrowVal -> Game
addThrow gs x = gs ++ [xx]
    where
        (yfn, yii, ytv, yfs) = if null gs then (0,0,0,Open) else last gs

        toNewFrame = yfs /= First && yfn /= 10
        isFill     = (yfs == Strike || yfs == Spare) && yfn == 10
        frameScore = xtv + ytv * fromEnum (not toNewFrame)

        xtv = x
        xii = yii + 1
        xfn = yfn + fromEnum toNewFrame
        xfs
            | x < 0            = Error
            | frameScore > 10  = Error
            | isFill           = Fill
            | frameScore == 10 = if toNewFrame then Strike else Spare
            | otherwise        = if toNewFrame then First  else Open

        xx = (xfn, xii, xtv, xfs)