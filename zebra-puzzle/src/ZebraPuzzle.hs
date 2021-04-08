module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Data.List ( permutations, findIndex )
import Control.Applicative ( Applicative(liftA2) )

data Resident  = Englishman | Spaniard | Ukrainian    | Norwegian   | Japanese    deriving (Eq, Show, Enum)
data Color     = Red        | Green    | Ivory        | Yellow      | Blue        deriving (Eq, Show, Enum)
data Pet       = Dog        | Snails   | Fox          | Horse       | Zebra       deriving (Eq, Show, Enum)
data Beverage  = Coffee     | Tea      | Milk         | Juice       | Water       deriving (Eq, Show, Enum)
data Cigarette = OldGold    | Kools    | Chesterfield | LuckyStrike | Parliaments deriving (Eq, Show, Enum)

data House = House { resident  :: Resident,
                     color     :: Color,
                     pet       :: Pet,
                     beverage  :: Beverage,
                     cigarette :: Cigarette } deriving (Eq, Show) 

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

type Candidate = [House]

residents  = [ Englishman , Spaniard , Ukrainian    , Norwegian   , Japanese ]
colors     = [ Red        , Green    , Ivory        , Yellow      , Blue ]
pets       = [ Dog        , Snails   , Fox          , Horse       , Zebra ]
beverages  = [ Coffee     , Tea      , Milk         , Juice       , Water ]
cigarettes = [ OldGold    , Kools    , Chesterfield , LuckyStrike , Parliaments ]

permResidents  = permutations [ Englishman , Spaniard , Ukrainian    , Norwegian   , Japanese ]
permColors     = permutations [ Red        , Green    , Ivory        , Yellow      , Blue ]
permPets       = permutations [ Dog        , Snails   , Fox          , Horse       , Zebra ]
permBeverages  = permutations [ Coffee     , Tea      , Milk         , Juice       , Water ]
permCigarettes = permutations [ OldGold    , Kools    , Chesterfield , LuckyStrike , Parliaments ]

myZip :: (a -> b -> c -> d -> e  -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
myZip f [] _ _ _ _ = []
myZip f _ [] _ _ _ = []
myZip f _ _ [] _ _ = []
myZip f _ _ _ [] _ = []
myZip f _ _ _ _ [] = []
myZip f (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e : myZip f as bs cs ds es 


solve :: Solution
solve = error "You need to implement this function."

candidates :: [Candidate]
candidates = [ myZip House rs cl pt bv cg | rs <- permResidents,
                                            cl <- permColors,
                                            pt <- permPets,
                                            bv <- permBeverages,
                                            cg <- permCigarettes ]

isSolution :: Candidate -> Bool
isSolution candidate = 
       any (\h -> resident  h == Englishman  && color     h == Red )         candidate -- 2
    && any (\h -> resident  h == Spaniard    && pet       h == Dog )         candidate -- 3
    && any (\h -> beverage  h == Coffee      && color     h == Green )       candidate -- 4
    && any (\h -> resident  h == Ukrainian   && beverage  h == Tea )         candidate -- 5
    && any (\h -> cigarette h == OldGold     && pet       h == Snails )      candidate -- 7
    && any (\h -> cigarette h == Kools       && color     h == Yellow )      candidate -- 8
    && any (\h -> cigarette h == LuckyStrike && beverage  h == Juice )       candidate -- 13
    && any (\h -> resident  h == Japanese    && cigarette h == Parliaments ) candidate -- 14
    && Milk      == beverage (candidate !! 2) -- 9
    && Norwegian == resident (head candidate) -- 10
    && liftA2 (-) (findIndex (\h -> color h     == Green)        candidate) (findIndex (\h -> color h == Ivory) candidate) == Just 1 -- 6
    && liftA2 (-) (findIndex (\h -> cigarette h == Chesterfield) candidate) (findIndex (\h -> pet h   == Fox)   candidate) `elem` [Just 1, Just (-1)] -- 11
    && liftA2 (-) (findIndex (\h -> cigarette h == Kools)        candidate) (findIndex (\h -> pet h   == Horse) candidate) `elem` [Just 1, Just (-1)] -- 12
    && liftA2 (-) (findIndex (\h -> resident h  == Norwegian)    candidate) (findIndex (\h -> color h == Blue)  candidate) `elem` [Just 1, Just (-1)] -- 15

solutions :: [Candidate]
solutions = filter isSolution candidates
