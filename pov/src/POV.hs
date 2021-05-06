module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(Node, rootLabel), Forest)
import Data.Maybe ( fromJust, listToMaybe )
import Control.Monad ( join )
import Data.List ( inits, tails )

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = inverse =<< findPath x tree

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = p2l path
    where
        tree' = fromPOV to tree
        path  = findPath from =<< tree'
        p2l   = fmap (map rootLabel)

inverse :: Forest a -> Maybe (Tree a)
inverse            []  = Nothing
inverse (Node a ls:[]) = Just (Node a ls)
inverse (Node a ls:ts) = Just (Node a ((fromJust . inverse $ ts) : ls))

findPath :: Eq a => a -> Tree a -> Maybe (Forest a)
findPath goal tree = error "lol"