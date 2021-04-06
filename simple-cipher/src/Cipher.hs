module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char ( toLower )
import System.Random ( newStdGen, randomRs )

alphaLen :: Int
alphaLen = length ['a'..'z']

intFromChar :: Char -> Int
intFromChar = subtract (fromEnum 'a') . fromEnum . toLower

charFromInt :: Int -> Char
charFromInt = toLower . toEnum . (+) (fromEnum 'a')

sumChar :: Char -> Char -> Char
sumChar x y = charFromInt ( ( intFromChar x + intFromChar y ) `mod` alphaLen)

subtractChar :: Char -> Char -> Char
subtractChar x y = charFromInt ( ( intFromChar x - intFromChar y ) `mod` alphaLen)

caesarDecode :: String -> String -> String
caesarDecode key = zipWith (flip subtractChar) (cycle key)

caesarEncode :: String -> String -> String
caesarEncode key = zipWith sumChar (cycle key)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
                          gen <- newStdGen
                          let key = take 10 (randomRs ('a', 'z') gen)
                          return (key, caesarEncode key text)