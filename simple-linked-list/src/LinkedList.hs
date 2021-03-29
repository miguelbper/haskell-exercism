module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil 
                  | New { datum :: a, next :: LinkedList a } deriving (Eq, Show)

instance Foldable LinkedList where
    foldr _ acc Nil = acc
    foldr f acc (New x ll) = f x (foldr f acc ll)

new :: a -> LinkedList a -> LinkedList a
new = New

nil :: LinkedList a
nil = Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

fromList :: [a] -> LinkedList a
fromList = foldr new nil

toList :: LinkedList a -> [a]
toList = foldr (:) []

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip new) nil