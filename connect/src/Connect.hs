module Connect (Mark(..), winner) where

import Data.Char ( isSpace )
import Data.List ( null )
import Data.Array ( Array, (!), indices, listArray )

type Pos   = (Int, Int)
type Path  = [Pos]
data Mark  = Cross | Nought deriving (Eq, Show)
type Board = Array Pos (Maybe Mark)

-- main functions
winner :: [String] -> Maybe Mark
winner xss
    | hasWon Cross  board = Just Cross
    | hasWon Nought board = Just Nought
    | otherwise           = Nothing
    where
        board = boardFromString xss

coord :: Mark -> (a,a) -> a
coord Cross  = snd
coord Nought = fst

hasWon :: Mark -> Board -> Bool
hasWon mark board = not . null $ do
    let crd   = coord mark
    let i     = indices board
    let (x,y) = last i
    let l     = crd (x,y)

    let init = filter (\a -> board ! a == Just mark) . filter (\a -> 1 == crd a) $ i
    let end  = filter (\a -> board ! a == Just mark) . filter (\a -> l == crd a) $ i

    a <- init

    let paths = filter (\x -> last x `elem` end)
              . extendPath board
              $ [a]

    paths

extendPath :: Board -> Path -> [Path]
extendPath board path = if null ext then [path] else concat [ extendPath board p | p <- ext ]
    where
        pos  = last path
        mark = board ! pos
        nbh  = filter (`notElem` path) 
             . filter ( (==) mark . (!) board ) 
             . filter (`elem` indices board) 
             . neighbours 
             $ pos
        
        ext = possibleExtensions path nbh
             
-- auxiliary functions
possibleExtensions :: [a] -> [a] -> [[a]]
possibleExtensions xs = map ( (++) xs . return )

neighbours :: Pos -> [Pos]
neighbours (x,y) = [ (x - 1, y    )
                   , (x - 1, y + 1) 
                   , (x    , y - 1)
                   , (x    , y + 1)
                   , (x + 1, y - 1)
                   , (x + 1, y    ) ]

-- convert
markFromChar :: Char -> Maybe Mark
markFromChar c = case c of
    'X' -> Just Cross
    'O' -> Just Nought
    _   -> Nothing 

boardFromString :: [String] -> Board
boardFromString xss = listArray ((1,1),(x,y)) val
    where
        xss' = map (filter (not . isSpace)) xss
        x    = length xss'
        y    = length (head xss')
        val  = map markFromChar . concat $ xss'