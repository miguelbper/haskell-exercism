{-# LANGUAGE TemplateHaskell #-}
module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Control.Monad ( liftM2 )
import Control.Lens (makeLenses, set, over)

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

type Month = Int

makeLenses ''Person
makeLenses ''Born
makeLenses ''Address

bornStreet :: Born -> String
bornStreet = _street . _bornAt

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Month -> Person -> Person
setBirthMonth = over born . over bornOn . monthChanger

renameStreets :: (String -> String) -> Person -> Person
renameStreets = liftM2 (.) (over address . over street) (over born . over bornAt . over street)

monthChanger :: Month -> Day -> Day
monthChanger m dy = let (y, _, d) = toGregorian dy in fromGregorian y m d