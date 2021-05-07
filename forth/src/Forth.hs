{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import qualified Data.Char as C (isDigit)
import Data.Text (Text, toLower, pack)
import qualified Data.Text as T (head)
import Data.Map (Map, fromList, insert)
import qualified Data.Map as M (lookup)
import Control.Applicative (Alternative((<|>)))
import Control.Monad (foldM)
import Data.Attoparsec.Text (sepBy, anyChar, Parser, parseOnly, takeWhile1, manyTill, skipSpace, choice, string, decimal, char, (<?>))

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- types
type Stack      = [Int]
type Operation  = Text
type Action     = Stack -> Either ForthError Stack
data ForthState = ForthState { stack   :: Stack, 
                               actions :: Map Operation Action}

-- list
toList :: ForthState -> [Int]
toList = reverse . stack

-- empty
emptyState :: ForthState
emptyState = ForthState [] defaultActions

defaultActions :: Map Operation Action
defaultActions = fromList [ ("+"   , binaryOp (+))
                          , ("-"   , binaryOp (-))
                          , ("*"   , binaryOp (*))
                          , ("/"   , division    )
                          , ("dup" , oneElemList (\x   -> [x,x]  ))
                          , ("drop", oneElemList (\x   -> []     ))
                          , ("swap", twoElemList (\x y -> [x,y]  ))
                          , ("over", twoElemList (\x y -> [x,y,x])) ]

binaryOp :: (Int -> Int -> Int) -> Action
binaryOp op (x:y:xs) = Right (op y x : xs)
binaryOp _  _        = Left StackUnderflow

division :: Action
division (0:y:xs) = Left DivisionByZero
division (x:y:xs) = Right (div y x : xs)
division _        = Left StackUnderflow

oneElemList :: (Int -> Stack) -> Action
oneElemList op (x:xs) = Right (op x ++ xs)
oneElemList _  _      = Left StackUnderflow

twoElemList :: (Int -> Int -> Stack) -> Action
twoElemList op (x:y:xs) = Right (op y x ++ xs)
twoElemList _  _        = Left StackUnderflow

-- eval
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = error "l"