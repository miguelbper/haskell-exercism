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
data FrameSc  = Error | First | Open | Spare | Strike deriving (Eq, Show)

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

        score
            | xfs == Strike = xtv + ytv + ztv
            | xfs == Spare  = xtv + ytv
            | otherwise     = xtv

        invRoll = Left (InvalidRoll xii xtv)

        result = liftM2 (+) (Right score) (score' xs)

complete :: Game -> Bool
complete = const True

-- convert
gameFromThrows :: Throws -> Game
gameFromThrows = foldl addThrow []

addThrow :: Game -> ThrowVal -> Game
addThrow gs x = gs ++ newFrame
    where
        newFrame = if null gs then [(1,1,x,s)] 
                              else [computeThrow (last gs) x]

        s | x < 0 || x > 10 = Error
          | x == 10         = Strike
          | otherwise       = First

computeThrow :: Frame -> ThrowVal -> Frame
computeThrow (yfn, yii, ytv, yfs) x = (xfn, xii, xtv, xfs)
    where
        xtv = x
        xii = yii + 1
        xfn = yfn + (if newFrame then 1 else 0)
        
        newFrame = yfs /= First && yfn /= 10

        yaa = if newFrame then 0 else ytv
        
        xfs | x < 0         = Error
            | x + yaa >  10 = Error
            | x + yaa == 10 = Spare
            | otherwise     = Open