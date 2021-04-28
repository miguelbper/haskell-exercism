module Poker (bestHands) where

import Control.Monad ( liftM2, join, (<=<) )
import Control.Arrow ( Arrow((***)), (&&&) ) 

-- datatypes
data Suit = S | D | H | C deriving (Eq)
data Rank = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | J | Q | K | A deriving (Eq, Ord, Enum)
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq)
--data Hand = Hand { c1 :: Card, c2 :: Card, c3 :: Card, c4 :: Card, c5 :: Card } deriving (Eq)
newtype Hand = Hand { listFromHand :: [Card] } deriving (Eq)
data HandRank = HighCard      Rank
              | OnePair       Rank
              | TwoPair       Rank
              | ThreeOfAKind  Rank
              | Straight      Rank
              | Flush         Rank
              | FullHouse     Rank
              | FourOfAKind   Rank
              | StraightFlush Rank deriving (Eq, Ord)

-- instances
-- this instance of Ord is not antisymmetric
instance Ord Card where
    (<=) = curry ( uncurry (<=) . ( rank *** rank ) )

-- instance Ord HandRank where


-- this instance of Ord is not antisymmetric
instance Ord Hand where
    (<=) = curry ( uncurry (<=) . ( handRank *** handRank ) )

-- main functions
bestHands :: [String] -> Maybe [String]
bestHands = fmap (return . stringFromHand . maximum) . mapM handFromString

handRank :: Hand -> HandRank
handRank hand = error "lol"
{-     | isStraightFlush hand = StraightFlush        
    | isFourOfAKind   hand = FourOfAKind      
    | isFullHouse     hand = FullHouse     
    | isFlush         hand = Flush 
    | isStraight      hand = Straight    
    | isThreeOfAKind  hand = ThreeOfAKind       
    | isTwoPair       hand = TwoPair  
    | isOnePair       hand = OnePair
    where
        isStraightFlush hand = True
        isFourOfAKind   hand = True
        isFullHouse     hand = True
        isFlush         hand = True
        isStraight      hand = True
        isThreeOfAKind  hand = True
        isTwoPair       hand = True
        isOnePair       hand = True -}

-- convert between hand and string
stringFromHand :: Hand -> String
stringFromHand = unwords
               . map ( uncurry (++)
                     . ( showRank . rank &&& showSuit . suit ) )
               . listFromHand

handFromString :: String -> Maybe Hand
handFromString = fmap Hand . mapM readCard . words

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