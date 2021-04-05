module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)
import qualified Prelude as P (null)
import qualified Data.List as L (sort)

type CustomSet a = [a]

delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete x set = difference set [x]

difference :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = filter (`notElem` setB) setA

empty :: CustomSet a
empty = []

fromList :: (Eq a, Ord a) => [a] -> CustomSet a
fromList = L.sort . foldr add []
    where
        add x l = if x `elem` l then l else x:l 

insert :: (Eq a, Ord a) => a -> CustomSet a -> CustomSet a
insert x set = fromList (x:set)

intersection :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
intersection setA setB = filter (`elem` setB) setA

isDisjointFrom :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom = curry (null . uncurry intersection)

isSubsetOf :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf setA setB = all (`elem` setB) setA

member :: (Eq a) => a -> CustomSet a -> Bool
member = elem

null :: CustomSet a -> Bool
null = P.null

size :: CustomSet a -> Int
size = length

toList :: CustomSet a -> [a]
toList = id

union :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union = curry (fromList . uncurry (++))
