module Connect (Mark(..), winner) where

import Data.Map ( Map, (!), member )

type Pos = (Int, Int)
type Path = [Pos]
data Mark = Cross | Nought deriving (Eq, Show)
type Board = Map Pos Mark

-- main functions
winner :: [String] -> Maybe Mark
winner xss
    | hasWon Cross  board = Just Cross
    | hasWon Nought board = Just Nought
    | otherwise           = Nothing
    where
        board = boardFromString xss

hasWon :: Mark -> Board -> Bool
hasWon = error "lol"

extendPath :: Board -> Path -> [Path]
extendPath _ [] = []
extendPath board path  = possibleExtensions path nbh
    where
        pos  = last path
        mark = board ! pos
        nbh  = filter (`notElem` path) 
             . filter ( (==) mark . (!) board ) 
             . filter (`member` board) 
             . neighbours 
             $ pos
             
-- auxiliary functions
possibleExtensions :: [a] -> [a] -> [[a]]
possibleExtensions xs = map ( (++) xs . return )

neighbours :: Pos -> [Pos]
neighbours (x,y) = [ (x - 1, y    )
                   , (x + 1, y    )
                   , (x    , y - 1)
                   , (x + 1, y - 1)
                   , (x    , y + 1)
                   , (x - 1, y + 1) ]

-- convert
boardFromString :: [String] -> Board
boardFromString = error "lol"