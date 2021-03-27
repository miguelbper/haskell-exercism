module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier = (`div` 2) . subtract 10

ability :: Gen Int
ability = ability' <$> roll <*> roll <*> roll <*> roll
  where
    roll = choose (1,6)

    ability' :: Int -> Int -> Int -> Int -> Int
    ability' a b c d = sum [a,b,c,d] - minimum [a,b,c,d]

character :: Gen Character
character = character' <$> ability <*> ability <*> ability <*> ability <*> ability <*> ability
  where
    character' :: Int -> Int -> Int -> Int -> Int -> Int -> Character
    character' str dex con int wis cha = Character str dex con int wis cha ((+ 10 ) . modifier $ con)