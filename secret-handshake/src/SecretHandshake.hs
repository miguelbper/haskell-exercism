module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = foldr handshake' [] nums
    where
        handshake' :: Int -> [String] -> [String]
        handshake' x xs
            | x == 1 = xs ++ ["wink"]
            | x == 2 = xs ++ ["double blink"]
            | x == 3 = xs ++ ["close your eyes"]
            | x == 4 = xs ++ ["jump"]
            | x == 5 = reverse xs
            | otherwise = xs 

        nums :: [Int]
        nums = reverse . zipWith (*) [1,2..] . take 5 . reverse . toBin $ n

        toBin :: Int -> [Int]
        toBin 0 = [0]
        toBin 1 = [1]
        toBin n
            | even n    = toBin (n `div` 2) ++ [0]
            | otherwise = toBin (n `div` 2) ++ [1]
