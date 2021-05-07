{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Char (isDigit, toUpper)
import Data.Bool
import Data.Text (Text, toLower, pack)
import qualified Data.Text as T (head, words, map, all, null)
import Data.Text.Read as TR (signed, decimal)
import Data.Map (Map, fromList, insert)
import qualified Data.Map as M (lookup, insert)
import Control.Applicative (Alternative((<|>)))
import Control.Monad (foldM, (>=>))
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
                          , ("DUP" , oneElemList (\x   -> [x,x]  ))
                          , ("DROP", oneElemList (\x   -> []     ))
                          , ("SWAP", twoElemList (\x y -> [x,y]  ))
                          , ("OVER", twoElemList (\x y -> [x,y,x])) ]

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
evalText text state = eval state (T.words text)

eval :: ForthState -> [Text] -> Either ForthError ForthState
eval st [] = Right st
eval st (w:ws) = case T.map toUpper w of
  ":" -> case break (";"==) ws of
           (ws',_:ws'') -> either Left (flip eval ws'') (updateDictWith ws' st)
           _            -> Left InvalidWord
  wd | T.all isDigit wd -> eval (st { stack = decimalToIntegral wd : stack st }) ws
     | otherwise        -> case M.lookup wd (actions st) of
                             Nothing -> Left $ UnknownWord w
                             Just f  -> either Left (flip eval ws) (updateStack f st)

updateStack :: ([Int] -> Either ForthError [Int]) -> ForthState -> Either ForthError ForthState
updateStack s st = either Left (Right . f) (s $ stack st)
  where
    f stk = st { stack = stk }

updateDictWith :: [Text] -> ForthState -> Either ForthError ForthState
updateDictWith wws st = case map (T.map toUpper) wws of
    w:ws | T.all isDigit w -> Left InvalidWord
         | otherwise       -> case mkop (actions st) ws of
             Left e  -> Left e
             Right f -> Right $ st { actions = M.insert (T.map toUpper w) f (actions st) }

mkop :: Map Text Action -> [Text] -> Either ForthError Action
mkop dic = foldr f (Right pure)
  where
    f x eop = if T.all isDigit x
              then composeOP (pure . (decimalToIntegral x :)) eop
              else case M.lookup x dic of
                     Nothing -> Left (UnknownWord x)
                     Just op -> composeOP op eop

decimalToIntegral :: Integral a => Text -> a
decimalToIntegral t = either (error . id)
                             (\ (i,rt) -> bool (error "invalid input") i (T.null rt))
                             (TR.signed TR.decimal t)

composeOP :: Action -> Either ForthError Action -> Either ForthError Action
composeOP op (Right op') = Right (op >=> op')
composeOp _  eop         = eop