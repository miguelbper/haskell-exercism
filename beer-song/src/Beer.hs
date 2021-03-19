module Beer (song) where

song :: String
song = concatMap song' [99, 98 .. 0]
  where
    song' :: Int -> String
    song' 0 =
      "No more bottles of beer on the wall, no more bottles of beer.\n"
        ++ "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    song' 1 =
      "1 bottle of beer on the wall, 1 bottle of beer.\n"
        ++ "Take it down and pass it around, no more bottles of beer on the wall.\n\n"
    song' 2 =
      "2 bottles of beer on the wall, 2 bottles of beer.\n"
        ++ "Take one down and pass it around, 1 bottle of beer on the wall.\n\n"
    song' x =
      show x ++ " bottles of beer on the wall, " ++ show x ++ " bottles of beer.\n"
        ++ "Take one down and pass it around, "
        ++ show (x - 1)
        ++ " bottles of beer on the wall.\n\n"