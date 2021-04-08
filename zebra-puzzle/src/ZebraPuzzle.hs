module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Monad ( guard )
import Data.List ( permutations, elemIndex )
import Control.Applicative ( Applicative(liftA2) )

data Resident  = Englishman | Spaniard | Ukrainian    | Norwegian   | Japanese    deriving (Eq, Show, Bounded, Enum)
data Color     = Red        | Green    | Ivory        | Yellow      | Blue        deriving (Eq, Show, Bounded, Enum)
data Pet       = Dog        | Snails   | Fox          | Horse       | Zebra       deriving (Eq, Show, Bounded, Enum)
data Beverage  = Coffee     | Tea      | Milk         | Juice       | Water       deriving (Eq, Show, Bounded, Enum)
data Cigarette = OldGold    | Kools    | Chesterfield | LuckyStrike | Parliaments deriving (Eq, Show, Bounded, Enum)

data House = House { resident  :: Resident,
                     color     :: Color,
                     pet       :: Pet,
                     beverage  :: Beverage,
                     cigarette :: Cigarette } deriving (Eq, Show) 

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = Solution waterResident zebraResident
    where
        waterResident = resident . head . filter ((==) Water . beverage) . head $ solutions
        zebraResident = resident . head . filter ((==) Zebra . pet)      . head $ solutions 

perm :: (Bounded a, Enum a) => [[a]]
perm = permutations [ minBound .. maxBound ]

myZip :: (a -> b -> c -> d -> e  -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
myZip _ [] _ _ _ _ = []
myZip _ _ [] _ _ _ = []
myZip _ _ _ [] _ _ = []
myZip _ _ _ _ [] _ = []
myZip _ _ _ _ _ [] = []
myZip f (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e : myZip f as bs cs ds es

solutions :: [[House]]
solutions = do
    resid <- (perm :: [[Resident]])
    guard ( Norwegian == head resid) -- 10

    color <- (perm :: [[Color]])
    guard ( (Englishman, Red) `elem` zip resid color ) -- 2
    guard ( liftA2 (-) (elemIndex Green color) (elemIndex Ivory color) == Just 1 ) -- 6
    guard ( liftA2 (-) (elemIndex Norwegian resid) (elemIndex Blue color) `elem` [Just 1, Just (-1)] ) -- 15

    pet   <- (perm :: [[Pet]])
    guard ( (Spaniard, Dog) `elem` zip resid pet) -- 3
    
    bevrg <- (perm :: [[Beverage]])
    guard ( (Coffee, Green) `elem` zip bevrg color) -- 4
    guard ( (Ukrainian, Tea) `elem` zip resid bevrg) -- 5
    guard ( Milk == bevrg !! 2 ) -- 9

    cigar <- (perm :: [[Cigarette]])
    guard ( (OldGold, Snails) `elem` zip cigar pet) -- 7
    guard ( (Kools, Yellow) `elem` zip cigar color) -- 8
    guard ( (LuckyStrike, Juice) `elem` zip cigar bevrg) -- 13
    guard ( (Japanese, Parliaments) `elem` zip resid cigar) -- 14
    guard ( liftA2 (-) (elemIndex Chesterfield cigar) (elemIndex Fox pet) `elem` [Just 1, Just (-1)] ) -- 11
    guard ( liftA2 (-) (elemIndex Kools cigar) (elemIndex Horse pet) `elem` [Just 1, Just (-1)] ) -- 12

    return (myZip House resid color pet bevrg cigar)
