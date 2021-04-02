module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite (x:ys) = firstLines (x:ys) ++ lastLine x
    where
        lastLine x = "And all for the want of a " ++ x ++ "."

        firstLines = concatMap line . break 2

        break :: Int -> [a] -> [[a]]
        break n ys
            | n > length ys = []
            | otherwise     = take n ys : break n (drop 1 ys)

        line :: [String] -> String
        line ys = "For want of a " ++ head ys ++ " the " ++ ys !! 1 ++ " was lost.\n"
