module Sublist (sublist) where

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
    | xs == ys        = Just EQ
    | isSublist xs ys = Just LT
    | isSublist ys xs = Just GT
    | otherwise       = Nothing
    where
        substrings :: Int -> [a] -> [[a]]
        substrings n cs
            | length cs <= n = [cs]
            | otherwise      = take n cs : substrings n (drop 1 cs) 

        isSublist :: Eq a => [a] -> [a] -> Bool
        isSublist aa bb = aa `elem` substrings (length aa) bb                
