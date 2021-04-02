module Clock (addDelta, fromHourMin, toString) where

import Data.Digits (digits) 

type Hour = Int
type Min  = Int

data Clock = Clock { hour :: Hour, min :: Min }
  deriving Eq

fromHourMin :: Hour -> Min -> Clock
fromHourMin hour min = addDelta hour min (Clock 0 0)

toString :: Clock -> String
toString (Clock hh mm) = toStr hh ++ ":" ++ toStr mm
  where
    toStr :: Int -> String
    toStr = map (toEnum . (+) 48) 
          . reverse . take 2 . reverse 
          . (:) 0 . (:) 0 
          . digits 10

addDelta :: Hour -> Min -> Clock -> Clock
addDelta hour min (Clock hh mm) = Clock hnew mnew
  where
    hnew = (hh + hour + ((mm + min) `div` 60)) `mod` 24
    mnew = (mm + min) `mod` 60