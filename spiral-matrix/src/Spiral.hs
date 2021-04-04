module Spiral (spiral) where

import Data.List ( transpose, elemIndex )
import Control.Monad ( liftM2 ) 
import Data.Maybe ( fromJust )

spiral :: Int -> [[Int]]
spiral 0 = []
spiral n = applyUntil isFilled (fill . rotate) . initMatrix $ n
    where
        isFilled :: [[Int]] -> Bool
        isFilled = liftM2 (&&) ((== 1) . head . head) (notElem 0 . concat)  

        initMatrix :: Int -> [[Int]]
        initMatrix k = [1..k] : replicate (k-1) (replicate k 0)

        rotate :: [[a]] -> [[a]]
        rotate = reverse . transpose

        applyUntil :: (a -> Bool) -> (a -> a) -> a -> a
        applyUntil b f i
            | b i = i
            | otherwise = applyUntil b f (f i)

        fill :: [[Int]] -> [[Int]]
        fill xss = [if maxn `elem` xs then update xs else xs | xs <- xss]
            where
                maxn      = maximum . map maximum $ xss
                lsum x y  = if x == 0 then y else x
                update xs = zipWith lsum xs [maxn - i,maxn + 1 - i..]
                    where
                        i = fromJust (elemIndex maxn xs)