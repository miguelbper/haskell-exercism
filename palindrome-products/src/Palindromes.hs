module Palindromes (largestPalindrome, smallestPalindrome) where

palindromes :: Integer -> Integer -> [(Integer, (Integer, Integer))]
palindromes x y = [(c, (a, b)) | a <- range,
                                 b <- range,
                                 let c = a * b,
                                 isPalindrome c ]
    where
        range = [x..y]

        digits :: Integral a => a -> [a]
        digits 0 = []
        digits n = digits (n `div` 10) ++ [n `mod` 10]

        isPalindrome :: Integer -> Bool
        isPalindrome m = digits m == reverse (digits m)


largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
    | null pal  = Nothing
    | otherwise = Just (value, factors)
    where
        pal     = palindromes minFactor maxFactor
        pal1    = map fst pal
        pal2    = map snd pal
        value   = maximum pal1
        factors = filter (\y -> uncurry (*) y == value) pal2

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
    | null pal  = Nothing
    | otherwise = Just (value, factors)
    where
        pal     = palindromes minFactor maxFactor
        pal1    = map fst pal
        pal2    = map snd pal
        value   = minimum pal1
        factors = filter (\y -> uncurry (*) y == value) pal2