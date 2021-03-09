module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum [ digit ^ nDigits | digit <- listOfDigits]
    where
        digs :: Integral x => x -> [x]
        digs 0 = []
        digs x = x `mod` 10 : digs (x `div` 10)

        listOfDigits = digs n
        nDigits = length listOfDigits
