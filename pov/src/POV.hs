module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node, rootLabel), Forest)
import Data.Maybe ( fromJust, listToMaybe, catMaybes )
import Data.List (delete)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = invert =<< paths [] x tree

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = p2l path
    where
        tree' = fromPOV to tree
        path  = paths [] from =<< tree'
        p2l   = fmap (map rootLabel)

invert :: Forest a -> Maybe (Tree a)
invert            []  = Nothing
invert (Node a ls:[]) = Just (Node a ls)
invert (Node a ls:ts) = Just (Node a ((fromJust . invert $ ts) : ls))

paths :: Eq a => Forest a -> a -> Tree a -> Maybe (Forest a)
paths f a (Node b ts) 
    | a == b    = Just (Node b ts : f)
    | otherwise = listToMaybe . catMaybes $ do
        t <- ts
        let ts' = delete t ts
        let f'  = Node b ts' : f
        return $ paths f' a t