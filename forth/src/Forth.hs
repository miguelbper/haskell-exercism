{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text)
import Data.Map (Map, fromList)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- types
type Stack      = [Int]
type Operation  = Text
type Action     = ForthState -> Either ForthError ForthState
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
                          , ("swap", twoElemList (\x y -> [y,x]  ))
                          , ("over", twoElemList (\x y -> [x,y,x])) ]

binaryOp :: (Int -> Int -> Int) -> Action
binaryOp op (ForthState (x:y:xs) act) = Right (ForthState (op y x : xs) act)
binaryOp _  _                         = Left StackUnderflow

division :: Action
division (ForthState (0:y:xs) act) = Left DivisionByZero
division (ForthState (x:y:xs) act) = Right (ForthState (div y x : xs) act)
division _                         = Left StackUnderflow

oneElemList :: (Int -> Stack) -> Action
oneElemList op (ForthState (x:xs) act) = Right (ForthState (op x ++ xs) act)
oneElemList _  _                       = Left StackUnderflow

twoElemList :: (Int -> Int -> Stack) -> Action
twoElemList op (ForthState (x:y:xs) act) = Right (ForthState (op x y ++ xs) act)
twoElemList _  _                         = Left StackUnderflow

-- eval
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text stack = error "You need to implement this function."

