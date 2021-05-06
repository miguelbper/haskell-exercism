module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set, empty, union, singleton)
import Data.List (nub)
import Data.Maybe (catMaybes)

data Color     = Empty | Black | White deriving (Eq, Ord, Show)
type Coord     = (Int, Int)
type Board     = [String]
type Territory = (Set Coord, Maybe Color)

-- main functions
territories :: Board -> [Territory]
territories board = nub . catMaybes $ [ territoryFor board (a,b) | a <- [1..x], b <- [1..y] ]
    where
        x = if null board then 0 else length (head board)
        y = length board

territoryFor :: Board -> Coord -> Maybe Territory
territoryFor board coord
    | coord `notElem` brd       = Nothing
    | clr `elem` [Black, White] = Nothing
    | otherwise                 = Just (nothingFromEmpty <$> territory board [] coord)  
    where
        x   = if null board then 0 else length (head board)
        y   = length board
        brd = [ (m,n) | m <- [1..x], n <- [1..y] ]
        clr = colorOfcoord board coord

territory :: Board -> [Coord] -> Coord -> Territory
territory board cs c
    | clr == White = (empty, Just White)
    | clr == Black = (empty, Just Black)
    | otherwise    = concatT (crdT : nbdT)    
    where
        clr = colorOfcoord board c
        nbd = filter (`notElem` cs) $ neighbours board c

        crdT = (singleton c, Just Empty)
        nbdT = map (territory board (c:cs)) nbd

-- sum territory
sumC :: Maybe Color -> Maybe Color -> Maybe Color
sumC x y
    | x == Just White = if y == Just White || y == Just Empty then Just White else Nothing
    | x == Just Black = if y == Just Black || y == Just Empty then Just Black else Nothing
    | x == Just Empty = y
    | otherwise       = Nothing

sumT :: Territory -> Territory -> Territory
sumT (a,b) (c,d) = (a `union` c, b `sumC` d)

concatT :: [Territory] -> Territory
concatT = foldr sumT (empty, Just Empty)

-- auxiliary functions
neighbours :: Board -> Coord -> [Coord]
neighbours board (a,b) = filter (`elem` brd) nbs
    where
        x   = if null board then 0 else length (head board)
        y   = length board
        brd = [ (m,n) | m <- [1..x], n <- [1..y] ]
        nbs = [ (a - 1, b    ),
                (a + 1, b    ),
                (a    , b - 1),
                (a    , b + 1) ]

toColor :: Char -> Color
toColor c = case c of
    'W' -> White
    'B' -> Black
    _   -> Empty

colorOfcoord :: Board -> Coord -> Color
colorOfcoord board (a,b) = toColor $ (board !! (b - 1)) !! (a - 1)

nothingFromEmpty :: Maybe Color -> Maybe Color
nothingFromEmpty (Just Empty) = Nothing
nothingFromEmpty x            = x