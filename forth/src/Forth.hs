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
type Action     = ForthState -> Either ForthError ForthState
data ForthState = ForthState { stack   :: Stack, 
                               actions :: Map Operation Action}

data Dictionary = Definition Text [Dictionary]
                | Number     Int
                | Word       Text
                deriving (Eq, Show)

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
twoElemList op (ForthState (x:y:xs) act) = Right (ForthState (op y x ++ xs) act)
twoElemList _  _                         = Left StackUnderflow

-- eval
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = case parseOnly parser text of
    Left  msg -> error msg
    Right dic -> foldM process state dic

process :: ForthState -> Dictionary -> Either ForthError ForthState
process s@(ForthState ls ops) (Number v) = return $ ForthState (v:ls) ops 
process s@(ForthState ls ops) (Word w) = case toLower w `M.lookup` ops of
    Just op -> op s
    Nothing -> Left (UnknownWord w)
process s@(ForthState ls ops) (Definition t act)
    | C.isDigit (T.head t) = Left InvalidWord -- XXX move to parser
    | otherwise = return $ ForthState ls (insert (toLower t) (flip (foldM process) act) ops)

parser :: Parser [Dictionary]
parser = (definition <|> computation) `sepBy` anyChar

computation :: Parser Dictionary
computation = choice [Number <$> decimal, Word <$> takeWhile1 (/=' ')]

definition :: Parser Dictionary
definition = Definition <$> (string ": " *> takeWhile1 (/=' ')) <*> manyTill (skipSpace *> computation) (skipSpace *> char ';') <?> "definition"