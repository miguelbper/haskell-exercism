module WordProblem (answer) where

import Data.Char ( isDigit )
import Control.Monad ( liftM2 )

answer :: String -> Maybe Integer
answer = compute . format . words

readNum :: String -> Maybe Integer
readNum x
    | isNum x   = Just $ read x
    | otherwise = Nothing

readOp :: (Integral a) => String -> Maybe (a -> a -> a)
readOp "plus"       = Just (+)
readOp "minus"      = Just (-)
readOp "multiplied" = Just (*)
readOp "divided"    = Just div
readOp _            = Nothing

compute :: [String] -> Maybe Integer
compute (      []) = Nothing
compute (    x:[]) = Just (read x)
compute (  _:_:[]) = Nothing
compute (x:y:z:xs) = do
    xx <- readNum x
    yy <- readOp  y
    zz <- readNum z
    let xyz = show (xx `yy` zz)
    compute (xyz:xs)

isNum :: String -> Bool
isNum = liftM2 (||) isPosNum isNegNum
    where
        isAllDigits = all isDigit
        isPosNum    = isAllDigits
        isNegNum    = liftM2 (&&) ( (>= 2) . length ) ( isAllDigits . drop 1 )

format :: [String] -> [String]
format = format2 . format1
    where
        format1    = map (filter (/= '?'))
        format2    = filter ( liftM2 (||) isNum isGoodWord )
        isGoodWord = flip notElem ["What", "is", "by"]