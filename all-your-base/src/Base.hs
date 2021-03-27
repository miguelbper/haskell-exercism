module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase  <= 1            = Left InvalidInputBase
    | outputBase <= 1            = Left InvalidOutputBase
    | not . null $ invalidDigits = Left (InvalidDigit (head invalidDigits))
    | otherwise                  = Right $ toOutputFromTen . toTenFromInput $ inputDigits
    where
        invalidDigits = filter (not . (`elem` [0..(inputBase - 1)])) inputDigits

        toTenFromInput ds = sum $ zipWith (*) (reverse ds) [ inputBase ^ n | n <- [0,1..] ]

        toOutputFromTen x
            | x == 0         = []
            | x < outputBase = [x]
            | otherwise      = toOutputFromTen (x `div` outputBase) ++ [x `mod` outputBase]

