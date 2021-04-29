module Poker (bestHands) where

import Data.Ord (comparing)
import Data.List ( group, groupBy, sort, sortOn, maximumBy ) 
import Control.Monad ( liftM2, join, (<=<) )
import Control.Arrow ( Arrow((***)), (&&&) ) 

-- datatypes
data Suit = S | D | H | C deriving (Eq, Show)
data Rank = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | J | Q | K | A deriving (Eq, Ord, Enum, Show)
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show)
type Hand = [Card]
data HandRank = HighCard      [Rank]
              | OnePair       Rank Rank Rank Rank
              | TwoPair       Rank Rank Rank
              | ThreeOfAKind  Rank Rank Rank
              | Straight      Rank
              | Flush         [Rank] 
              | FullHouse     Rank Rank
              | FourOfAKind   Rank Rank
              | StraightFlush Rank 
              deriving (Eq, Ord, Show)

-- instances this instance of Ord is not antisymmetric
instance Ord Card where
    (<=) = curry ( uncurry (<=) . ( rank *** rank ) )
    (>=) = curry ( uncurry (>=) . ( rank *** rank ) )

-- main functions
bestHands :: [String] -> Maybe [String]
bestHands = fmap ( map stringFromHand 
                 . maximumBy (\xs ys -> compare (handRank . head $ xs) (handRank . head $ ys) )
                 . groupBy (\c1 c2 -> handRank c1 == handRank c2) ) 
          . mapM handFromString


handRank :: Hand -> HandRank
handRank hand
    | strHi && flush = StraightFlush (last rankList)
    | strLo && flush = StraightFlush (last . init $ rankList)
    | flush          = Flush         (map rank . reverse . sort $ hand)
    | strHi          = Straight      (last rankList)
    | strLo          = Straight      (last . init $ rankList)
    | otherwise      = case rankGrps of
        [[g],[h,_,_,_]]     -> FourOfAKind h g
        [[g,_],[h,_,_]]     -> FullHouse h g
        [[g],[h],[i,_,_]]   -> ThreeOfAKind i h g
        [[g],[h,_],[i,_]]   -> TwoPair i h g
        [[g],[h],[i],[j,_]] -> OnePair j i h g
        _                   -> HighCard (map rank . reverse . sort $ hand)
    where
        rankList = sort . map rank $ hand
        rankGrps = sortOn length . group $ rankList

        flush = allEqual . map suit $ hand
        strHi = ascending rankList
        strLo = ascending (init rankList) && last rankList == A

-- helper functions
ascending :: (Enum a, Eq a) => [a] -> Bool
ascending xs = all (\(x,y) -> succ x == y) (zip xs (tail xs))

allEqual :: Eq a => [a] -> Bool
allEqual = and . (zipWith (==) <*> tail)

-- convert between hand and string
stringFromHand :: Hand -> String
stringFromHand = unwords
               . map ( uncurry (++)
                     . ( showRank . rank &&& showSuit . suit ) )

handFromString :: String -> Maybe Hand
handFromString = mapM readCard . words
    where
        readCard :: String -> Maybe Card
        readCard xs = do
            let i = init xs
            let l = return (last xs)
            r <- readRank i
            s <- readSuit l
            return $ Card r s

-- read and show
showSuit :: Suit -> String
showSuit suit = case suit of
    S -> "S"
    D -> "D"
    H -> "H"
    C -> "C"

readSuit :: String -> Maybe Suit
readSuit xs = case xs of
    "S" -> Just S
    "D" -> Just D
    "H" -> Just H
    "C" -> Just C
    _   -> Nothing

showRank :: Rank -> String
showRank rank = case rank of
    N2  -> "2"
    N3  -> "3"
    N4  -> "4"
    N5  -> "5"
    N6  -> "6"
    N7  -> "7"
    N8  -> "8"
    N9  -> "9"
    N10 -> "10"
    J   -> "J"
    Q   -> "Q"
    K   -> "K"
    A   -> "A"

readRank :: String -> Maybe Rank
readRank xs = case xs of
    "2"  -> Just N2
    "3"  -> Just N3
    "4"  -> Just N4
    "5"  -> Just N5
    "6"  -> Just N6
    "7"  -> Just N7
    "8"  -> Just N8
    "9"  -> Just N9
    "10" -> Just N10
    "J"  -> Just J
    "Q"  -> Just Q
    "K"  -> Just K
    "A"  -> Just A
    _    -> Nothing