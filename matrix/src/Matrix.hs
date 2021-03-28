module Matrix
    ( Matrix
    , cols
    , rows
    , flatten
    , shape
    , column
    , row
    , transpose
    , reshape
    , fromList
    , fromString
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Arrow ( Arrow((&&&)) )
import Data.List.Split (chunksOf)
import qualified Data.List as L
import Data.Maybe (listToMaybe)

data Matrix a = Matrix { rows    :: Int,
                         cols    :: Int,
                         flatten :: Vector a } deriving (Eq, Show)

shape :: Matrix a -> (Int, Int)
shape = rows &&& cols

column :: Int -> Matrix a -> Vector a
column x = row x . transpose

row :: Int -> Matrix a -> Vector a
row x matrix = V.fromList (toList matrix !! (x-1))

transpose :: Matrix a -> Matrix a
transpose = fromList . L.transpose . toList 

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix _ _ v) = Matrix r c v

toList :: Matrix a -> [[a]]
toList (Matrix _ c v) = chunksOf c . V.toList $ v

fromList :: [[a]] -> Matrix a
fromList l = if null l
             then Matrix 0 0 V.empty
             else Matrix (length l) (length (head l)) (V.fromList (concat l))

fromString :: Read a => String -> Matrix a
fromString = fromList . map (L.unfoldr (listToMaybe . reads)) . lines