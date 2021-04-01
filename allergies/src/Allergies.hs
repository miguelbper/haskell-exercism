module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Digits (digits) 

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum)

allergies :: Int -> [Allergen]
allergies = map (toEnum . subtract 1)
          . filter (/= 0)
          . zipWith (*) [1,2..]
          . take 8
          . reverse
          . digits 2

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
