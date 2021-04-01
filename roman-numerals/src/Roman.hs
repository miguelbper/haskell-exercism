module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n 
    | n <= 0    = Nothing 
    | otherwise = Just (concatMap (uncurry romanFromDigit) . toListOfPairs . digs $ n)
    where
        digs :: Integer -> [Int]
        digs 0 = []
        digs x = digs (x `div` 10) ++ [fromIntegral x `mod` 10]

        toListOfPairs :: [Int] -> [(Int, Int)]
        toListOfPairs = reverse . zip [0,1..] . take 4 . reverse

        romanFromDigit :: Int -> Int -> String
        romanFromDigit k d = case d of
                                  0 -> ""
                                  1 -> one
                                  2 -> one  ++ one
                                  3 -> one  ++ one ++ one
                                  4 -> one  ++ five
                                  5 -> five
                                  6 -> five ++ one
                                  7 -> five ++ one ++ one
                                  8 -> five ++ one ++ one ++ one
                                  9 -> one  ++ ten
                             
            where
                one  = case k of 
                            0 -> "I"
                            1 -> "X"
                            2 -> "C"
                            3 -> "M"

                five = case k of 
                            0 -> "V"
                            1 -> "L"
                            2 -> "D"
                            3 -> ""

                ten  = case k of 
                            0 -> "X"
                            1 -> "C"
                            2 -> "M"
                            3 -> ""