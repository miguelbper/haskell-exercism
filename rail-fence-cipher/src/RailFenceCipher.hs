module RailFenceCipher (encode, decode) where

import Data.List ( transpose )
import Data.List.Split ( chunksOf )
import Data.Text as T ( Text, replace, pack, unpack, null, drop )

rail :: Int -> [String]
rail n = cycle [ rail' n i | i <- [0..(2 * n - 1)] ]
    where
        k :: Int -> Int -> Int
        k n x = 2 * n * ( ( x + n) `div` (2 * n) )

        f :: Int -> Int -> Int
        f n x = abs ( x - k n x)

        rail' :: Int -> Int -> String
        rail' n x = replicate (f n x) '.' ++ "*" ++ replicate (n - f n x) '.'

encode :: Int -> String -> String
encode n = concatMap (filter (/= '.'))
         . transpose
         . zipWith (flip replace') (rail (n - 1))
         . filter (/= ' ')
    where
        replace' :: Char -> String -> String
        replace' c xs = unpack (replace (pack "*") (pack [c]) (pack xs))


decode :: Int -> String -> String
decode n xs = concatMap (filter (/= '.'))
            . transpose
            . chunksOf ln
            $ foldl (flip replace1) rl xs
                where
                    ln = length xs
                    rl = concat . transpose . take ln $ rail (n - 1)
                    
                    replace1 :: Char -> String -> String
                    replace1 c = mapOnce check
                        where
                            check x 
                                | x == '*'  = Just c
                                | otherwise = Nothing
                        
                    mapOnce :: (a -> Maybe a) -> [a] -> [a]
                    mapOnce _ []     = []
                    mapOnce g (x:xs) = case g x of
                            Nothing -> x : mapOnce g xs
                            Just y  -> y : xs