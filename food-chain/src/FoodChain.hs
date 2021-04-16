module FoodChain (song) where

type Animal = String
type Lyric  = String

animals :: [Animal]
animals = ["cow","goat","dog","cat","bird","spider","fly"]

firstLyric :: Animal -> Lyric
firstLyric a = "I know an old lady who swallowed a " ++ a ++ ".\n"

specialLyric :: Animal -> Lyric
specialLyric a = case a of
    "cow"    -> "I don't know how she swallowed a cow!\n"
    "goat"   -> "Just opened her throat and swallowed a goat!\n"
    "dog"    -> "What a hog, to swallow a dog!\n"
    "cat"    -> "Imagine that, to swallow a cat!\n"
    "bird"   -> "How absurd to swallow a bird!\n"
    "spider" -> "It wriggled and jiggled and tickled inside her.\n"
    _        -> ""

spiderLyric :: Animal -> Lyric
spiderLyric "spider" = "spider that wriggled and jiggled and tickled inside her"
spiderLyric a        = a

swallowedLyric :: Animal -> Animal -> Lyric
swallowedLyric a b = "She swallowed the " ++ a ++ " to catch the " ++ spiderLyric b ++ ".\n"

endLyric :: [Animal] -> Lyric
endLyric []       = ""
endLyric ["fly"]  = "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"
endLyric [_]      = ""
endLyric (a:b:as) = swallowedLyric a b ++ endLyric (b:as)

songBlock :: [Animal] -> Lyric
songBlock []     = ""
songBlock (a:as) = firstLyric a ++ specialLyric a ++ endLyric (a:as)

lastLyric :: Lyric
lastLyric = "I know an old lady who swallowed a horse.\nShe's dead, of course!\n"

song :: String
song = concatMap songBlock ( listOfLast  animals ) ++ lastLyric

listOfLast :: [a] -> [[a]]
listOfLast []     = []
listOfLast (x:xs) = listOfLast xs ++ [x:xs]