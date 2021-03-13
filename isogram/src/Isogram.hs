module Isogram (isIsogram) where

import Data.Text as T (Text, filter, length, all)
import Data.Char as C (toLower)
import Control.Monad ( liftM2 )

isIsogram :: Text -> Bool
isIsogram xs = T.all (liftM2 (||) canRepeat isOnly) xs
    where 
        canRepeat :: Char -> Bool
        canRepeat ' ' = True
        canRepeat '-' = True
        canRepeat _   = False

        isOnly :: Char -> Bool
        isOnly c = 1 == T.length (T.filter ((== C.toLower c) . C.toLower) xs)