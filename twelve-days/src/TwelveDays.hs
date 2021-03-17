module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = [ recite' x | x <- [start..stop] ]
    where
        recite' :: Int -> String
        recite' n = "On the " 
                    ++ ordinal !! (n - 1) 
                    ++ " day of Christmas my true love gave to me: " 
                    ++ concat (reverse (take n gifts))

        gifts :: [String]
        gifts = ["a Partridge in a Pear Tree.",
                 "two Turtle Doves, and ",
                 "three French Hens, ",
                 "four Calling Birds, ",
                 "five Gold Rings, ", 
                 "six Geese-a-Laying, ", 
                 "seven Swans-a-Swimming, ", 
                 "eight Maids-a-Milking, ", 
                 "nine Ladies Dancing, ", 
                 "ten Lords-a-Leaping, ", 
                 "eleven Pipers Piping, ", 
                 "twelve Drummers Drumming, "]

        ordinal :: [String]
        ordinal = ["first",
                   "second", 
                   "third",
                   "fourth",
                   "fifth", 
                   "sixth", 
                   "seventh", 
                   "eighth", 
                   "ninth",
                   "tenth",
                   "eleventh",
                   "twelfth"]
