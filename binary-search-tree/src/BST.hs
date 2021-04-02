module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Nil
           | BST { value :: a,
                   left  :: BST a,
                   right :: BST a} deriving (Eq, Show)

instance Foldable BST where
    foldr _ acc Nil         = acc
    foldr f acc (BST a l r) = foldr f (f a (foldr f acc r)) l

bstLeft :: BST a -> Maybe (BST a)
bstLeft Nil  = Nothing
bstLeft tree = Just . left $ tree

bstRight :: BST a -> Maybe (BST a)
bstRight Nil  = Nothing
bstRight tree = Just . right $ tree

bstValue :: BST a -> Maybe a
bstValue Nil  = Nothing
bstValue tree = Just . value $ tree

empty :: BST a
empty = Nil

singleton :: a -> BST a
singleton x = BST x Nil Nil

insert :: Ord a => a -> BST a -> BST a
insert x Nil = singleton x
insert x (BST y l r)
    | x <= y    = BST y (insert x l) r
    | x >  y    = BST y l (insert x r)
    | otherwise = BST y l r

toList :: BST a -> [a]
toList = foldr (:) []

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty