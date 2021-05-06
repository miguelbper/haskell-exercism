module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

import Data.Maybe ( fromJust, isNothing )

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

data Breadcrumb a  = LeftCrumb a (Maybe (BinTree a)) | RightCrumb a (Maybe (BinTree a)) deriving (Eq, Show)
type Breadcrumbs a = [Breadcrumb a]
type Zipper a      = (BinTree a, Breadcrumbs a)

fromTree :: BinTree a -> Zipper a
fromTree tree = (tree, [])

toTree :: Zipper a -> BinTree a
toTree = fst . toTree'

toTree' :: Zipper a -> Zipper a
toTree' (t, []) = (t,[])
toTree' z = toTree' . fromJust . up $ z

value :: Zipper a -> a
value = btValue . fst

left :: Zipper a -> Maybe (Zipper a)
left (BT x l r, bs)
    | isNothing l = Nothing
    | otherwise   = Just (fromJust l, LeftCrumb x r : bs)

right :: Zipper a -> Maybe (Zipper a)
right (BT x l r, bs)
    | isNothing r = Nothing
    | otherwise   = Just (fromJust r, RightCrumb x l : bs)

up :: Zipper a -> Maybe (Zipper a)
up (_, []) = Nothing
up (t, (LeftCrumb  x r):bs) = Just (BT x (Just t) r ,bs)
up (t, (RightCrumb x l):bs) = Just (BT x l (Just t) ,bs)

setValue :: a -> Zipper a -> Zipper a
setValue x (BT _ l r, bs) = (BT x l r, bs)

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft x (BT a _ r, bs) = (BT a x r, bs)

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight x (BT a l _, bs) = (BT a l x, bs)