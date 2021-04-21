module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Control.Arrow ( (&&&) )
import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar, modifyMVar_ )
import Safe ( headMay, lastMay )

type Deque a = MVar [a]

mkDeque :: IO (Deque a)
mkDeque = newMVar []

pop :: Deque a -> IO (Maybe a)
pop = flip modifyMVar $ return . (drop 1 &&& headMay)

push :: Deque a -> a -> IO ()
push deque x = modifyMVar_ deque $ return . (:) x

unshift :: Deque a -> a -> IO ()
unshift deque x = modifyMVar_ deque $ return . ( ++ [x] )

shift :: Deque a -> IO (Maybe a)
shift = flip modifyMVar $ return . (init &&& lastMay)
