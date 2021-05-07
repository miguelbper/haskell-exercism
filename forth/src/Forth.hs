{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Prelude hiding (lookup, words, all)
import Data.Char      (isDigit)
import Data.Text      (Text, words, all, toLower)
import Data.Text.Read (decimal)
import Data.Map       (Map, fromList, lookup, insert)
import Data.Maybe     (isJust, fromJust) 
import Data.Either    (fromRight)
import Control.Monad  ((>=>))

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
                          , ("drop", oneElemList (\_   -> []     ))
                          , ("swap", twoElemList (\x y -> [x,y]  ))
                          , ("over", twoElemList (\x y -> [x,y,x])) ]

binaryOp :: (Int -> Int -> Int) -> Action
binaryOp op (x:y:xs) = Right (op y x : xs)
binaryOp _  _        = Left StackUnderflow

division :: Action
division (0:_:_ ) = Left DivisionByZero
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
evalText text = eval (words . toLower $ text) 

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
        numb = all isDigit w
        word = isJust act

        act  = lookup w (actions state)
        act' = fromJust act

updateWithNumb :: Int -> ForthState -> ForthState
updateWithNumb x (ForthState state acts) = ForthState (x:state) acts

updateWithWord :: (Stack -> EStack) -> ForthState -> EForthState
updateWithWord action (ForthState stk acts) = fmap f estack
    where
        estack = action stk
        f sstk = ForthState sstk acts

updateWithDeft :: [Text] -> ForthState -> EForthState
updateWithDeft wws (ForthState stk acts)
    | numb      = Left InvalidWord
    | otherwise = case mkop acts ws of
        Left  err -> Left err
        Right act -> Right (ForthState stk (insert w act acts))
    where
        w    = head wws
        ws   = drop 1 wws
        numb = all isDigit w

mkop :: ActionMap -> [Text] -> EAction
mkop dic = foldr (mkopF dic) (Right pure)

mkopF :: ActionMap -> Text -> EAction -> EAction
mkopF dic x eop
    | numb       = (fmap . (>=>)) (pure . (intFromText x :)) eop
    | otherwise  = case lookup x dic of
        Nothing -> Left (UnknownWord x)
        Just op -> (fmap . (>=>)) op eop
    where
        numb = all isDigit x

intFromText :: Text -> Int
intFromText = fst . fromRight (0,"") . decimal