module ResistorColors (Color(..), Resistor(..), label, ohms) where

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
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

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

proj :: Int -> (a,a,a) -> a
proj 1 (x,_,_) = x
proj 2 (_,x,_) = x
proj 3 (_,_,x) = x

label :: Resistor -> String
label resistor
  | ohms' >= 10^9 = show ohmsGiga ++ " gigaohms"  
  | ohms' >= 10^6 = show ohmsMega ++ " megaohms"
  | ohms' >= 10^3 = show ohmsKilo ++ " kiloohms"
  | otherwise     = show ohms'    ++ " ohms"
  where
    ohms'    = ohms resistor
    ohmsGiga = ohms' `div` 10^9
    ohmsMega = ohms' `div` 10^6
    ohmsKilo = ohms' `div` 10^3

ohms :: Resistor -> Int
ohms resistor = (n1 * 10 + n2) * 10^n3
  where
    n1 = cd . proj 1 . bands $ resistor
    n2 = cd . proj 2 . bands $ resistor
    n3 = cd . proj 3 . bands $ resistor
