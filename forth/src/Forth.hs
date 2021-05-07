{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Char (isDigit, toUpper)
import qualified Data.Char as C (toLower)
import Data.Bool ( bool )
import Data.Text (Text )
import qualified Data.Text as T (words, map, all, null, toLower)
import Data.Text.Read as TR (signed, decimal)
import Data.Map (Map, fromList, insert)
import qualified Data.Map as M (lookup, insert)
import Control.Applicative (Alternative((<|>)))
import Control.Monad (foldM, (>=>), (<=<), join, liftM2)
import Data.Attoparsec.Text (sepBy, anyChar, Parser, parseOnly, takeWhile1, manyTill, skipSpace, choice, string, decimal, char, (<?>))
import Data.Maybe ( isJust, fromJust ) 
import Data.Either (fromRight)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- types
type Stack      = [Int]
type Operation  = Text
type Action     = Stack -> EStack
type ActionMap  = Map Operation Action
data ForthState = ForthState { stack   :: Stack, 
                               actions :: ActionMap }

type EStack      = Either ForthError Stack
type EAction     = Either ForthError Action
type EForthState = Either ForthError ForthState

-- list
toList :: ForthState -> Stack
toList = reverse . stack

-- empty
emptyState :: ForthState
emptyState = ForthState [] defaultActions

defaultActions :: ActionMap
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
evalText :: Text -> ForthState -> EForthState
evalText text = eval (T.words . T.toLower $ text) 

eval :: [Text] -> ForthState -> EForthState
eval []     state = Right state
eval (w:ws) state
    | deft      = case break (";"==) ws of
        (xs, [_]) -> updateWithDeft xs state
        _         -> Left InvalidWord
    | numb      = eval ws (updateWithNumb (intFromText w) state)
    | word      = eval ws =<< updateWithWord act' state
    | otherwise = Left $ UnknownWord w
    where
        deft = w == ":"
        numb = T.all isDigit w
        word = isJust act

        act  = M.lookup w (actions state)
        act' = fromJust act

updateWithNumb :: Int -> ForthState -> ForthState
updateWithNumb x (ForthState state actions) = ForthState (x:state) actions

updateWithWord :: (Stack -> EStack) -> ForthState -> EForthState
updateWithWord action state = fmap f estack
    where
        estack = action (stack state)
        f stk  = state { stack = stk }

updateWithDeft :: [Text] -> ForthState -> EForthState
updateWithDeft wss state = case wss of
    w:ws | T.all isDigit w -> Left InvalidWord
         | otherwise       -> case mkop (actions state) ws of
             Left e  -> Left e
             Right f -> Right $ state { actions = M.insert w f (actions state) }

mkop :: ActionMap -> [Text] -> EAction
mkop dic = foldr f (Right pure)
  where
    f x eop = if T.all isDigit x
              then composeOp (pure . (intFromText x :)) eop
              else case M.lookup x dic of
                     Nothing -> Left (UnknownWord x)
                     Just op -> composeOp op eop

-- auxiliary
intFromText :: Text -> Int
intFromText = fst . fromRight (0,"") . TR.decimal

composeAction :: Action -> Action -> Action
composeAction f g = g <=< f

composeOp :: Action -> EAction -> EAction
composeOp = fmap . composeAction