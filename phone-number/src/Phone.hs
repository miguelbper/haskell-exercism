module Phone (number) where

number :: String -> Maybe String
number xs
    | valid     = Just str'
    | otherwise = Nothing
    where
        xstr = ['0'..'9']
        nstr = ['2'..'9']
        
        str  = filter (`elem` xstr) xs
        str' = reverse . take 10 . reverse $ str
        
        len = length str
        
        valid =    (head str' `elem` nstr)
                && (str' !! 3 `elem` nstr)
                && (len == 10 || (len == 11 && head str == '1'))
