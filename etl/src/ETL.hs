module ETL (transform) where

import Data.Map as M (Map, keys, filter, fromList)
import Data.Char as C (toUpper)
transform :: Map a String -> Map Char a
transform legacyData = M.fromList [ (c, x) | c <- ['a'..'z'], x <- (M.keys . M.filter (elem (C.toUpper c))) legacyData]
