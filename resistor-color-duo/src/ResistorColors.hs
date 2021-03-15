module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded)

value :: (Color, Color) -> Int
value (a, b) = 10 * cd a + cd b
  where 
    cd :: Color -> Int
    cd Black  = 0
    cd Brown  = 1
    cd Red    = 2
    cd Orange = 3
    cd Yellow = 4
    cd Green  = 5
    cd Blue   = 6
    cd Violet = 7
    cd Grey   = 8
    cd White  = 9