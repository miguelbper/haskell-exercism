module Anagram (anagramsFor) where

import Data.List ( sort )
import Data.Char ( toLower )
import Control.Monad ( liftM2 )

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter $ liftM2 (&&) isAnagram notSame
    where
        isAnagram :: String -> Bool
        isAnagram = (==) (sort . map toLower $ xs) . sort . map toLower

        notSame :: String -> Bool
        notSame = (/=) (map toLower xs) . map toLower