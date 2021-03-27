module Brackets (arePaired) where
    
arePaired :: String -> Bool
arePaired = null . foldl f [] . filter (`elem` "(){}[]")
    where
        f :: String -> Char -> String
        f ('(':xs) ')' = xs
        f ('{':xs) '}' = xs
        f ('[':xs) ']' = xs
        f xs c         = c:xs