module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day(..),
                           fromGregorian, 
                           gregorianMonthLength,
                           addDays,
                           )

import qualified Data.Time.Calendar as T (DayOfWeek(..), dayOfWeek)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

instance Enum Weekday where
    toEnum i =
        case mod i 7 of
            0 -> Sunday
            1 -> Monday
            2 -> Tuesday
            3 -> Wednesday
            4 -> Thursday
            5 -> Friday
            _ -> Saturday

    fromEnum Monday    = 1
    fromEnum Tuesday   = 2
    fromEnum Wednesday = 3
    fromEnum Thursday  = 4
    fromEnum Friday    = 5
    fromEnum Saturday  = 6
    fromEnum Sunday    = 7

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

dayOfWeek' :: Day -> Weekday
dayOfWeek' = toEnum . fromEnum . T.dayOfWeek 

dayOfWeekDiff' :: Weekday -> Weekday -> Int
dayOfWeekDiff' a b = (fromEnum a - fromEnum b) `mod` 7

firstDayOfWeekOnAfter' :: Weekday -> Day -> Day
firstDayOfWeekOnAfter' dw d = addDays (toInteger $ dayOfWeekDiff' dw $ dayOfWeek' d) d

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = firstDayOfWeekOnAfter' weekday day
    where
        day = case schedule of
            First  -> fromGregorian year month 1 
            Second -> fromGregorian year month 8  
            Third  -> fromGregorian year month 15 
            Fourth -> fromGregorian year month 22  
            Last   -> fromGregorian year month (gregorianMonthLength year month - 6)
            Teenth -> fromGregorian year month 13