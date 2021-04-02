module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = flip (++) "\n"
      . intercalate "\n\n"
      . map (flip (++) "." . rhyme')
      . listOfLasts
      $ lyrics
    where
        nouns :: [String]
        nouns = ["horse and the hound and the horn",
                 "farmer sowing his corn",
                 "rooster that crowed in the morn",
                 "priest all shaven and shorn",
                 "man all tattered and torn",
                 "maiden all forlorn",
                 "cow with the crumpled horn",
                 "dog",
                 "cat",
                 "rat",
                 "malt",
                 "house that Jack built"]

        verbs :: [String]
        verbs = ["is",
                 "belonged to",
                 "kept",
                 "woke",
                 "married",
                 "kissed",
                 "milked",
                 "tossed",
                 "worried",
                 "killed",
                 "ate",
                 "lay in"]

        lyrics :: [(String, String)]
        lyrics = zip verbs nouns

        last' :: Int -> [a] -> [a]
        last' n = reverse . take n . reverse

        listOfLasts :: [a] -> [[a]]
        listOfLasts xs = [ last' n xs | n <- [1..(length xs)]]

        rhyme' :: [(String, String)] -> String
        rhyme' [] = ""
        rhyme' (x:xs) = firstLine x ++ concatMap otherLine xs
            where
                firstLine :: (String, String) -> String
                firstLine (_,n) = "This is the " ++ n

                otherLine :: (String, String) -> String
                otherLine (v,n) = "\nthat " ++ v ++ " the " ++ n