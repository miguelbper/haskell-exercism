module Diamond (diamond) where

import Data.Char ( toUpper, ord, chr )

diamond :: Char -> Maybe [String]
diamond c
    | toUpper c `notElem` ['A'..'Z'] = Nothing
    | otherwise                      = (Just . map (map itc)) ((upDiamond . cti) c ++ (reverse . init . upDiamond . cti) c)
    where
        equator :: Int -> [Int]
        equator 1 = [1]
        equator x = [x] ++ replicate (2 * x - 3) 0 ++ [x]

        upDiamond :: Int -> [[Int]]
        upDiamond 1 = [[1]]
        upDiamond x = [ [0] ++ xs ++ [0] | xs <- upDiamond (x-1) ] ++ [equator x]

        cti :: Char -> Int
        cti ' ' = 0
        cti c   = (ord . toUpper) c - 64

        itc :: Int -> Char
        itc 0 = ' '
        itc x = chr (x + 64)