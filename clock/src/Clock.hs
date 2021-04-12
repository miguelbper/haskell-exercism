module Clock (addDelta, fromHourMin, toString) where

import Text.Printf ( printf )

type Hour = Int
type Min  = Int

data Clock = Clock Hour Min deriving Eq

fromHourMin :: Hour -> Min -> Clock
fromHourMin hh mm = addDelta hh mm (Clock 0 0)

toString :: Clock -> String
toString (Clock hh mm) = printf "%02d:%02d" hh mm

addDelta :: Hour -> Min -> Clock -> Clock
addDelta hour mnt (Clock hh mm) = Clock hnew mnew
  where
    hnew = (hh + hour + ((mm + mnt) `div` 60)) `mod` 24
    mnew = (mm + mnt) `mod` 60