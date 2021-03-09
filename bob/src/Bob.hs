module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
    | isSilent                = "Fine. Be that way!"
    | isYelling && isQuestion = "Calm down, I know what I'm doing!"
    | isYelling               = "Whoa, chill out!"
    | isQuestion              = "Sure."
    | otherwise               = "Whatever."
    where
        xs'        = filter (not . isSpace) xs
        isSilent   = null xs'
        isQuestion = last xs' == '?'
        isYelling  = not (any isLower xs') && any isLetter xs'