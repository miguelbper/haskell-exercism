module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs
    | all isNucleotide xs = Right theRNAString
    | otherwise = Left firstNotNucleotide
    where
        isNucleotide :: Char -> Bool
        isNucleotide x = x `elem` ['A','C','G','T']

        toRNAChar :: Char -> Char
        toRNAChar 'G' = 'C'
        toRNAChar 'C' = 'G'
        toRNAChar 'T' = 'A'
        toRNAChar 'A' = 'U'
        toRNAChar x   = x

        theRNAString = map toRNAChar xs
        firstNotNucleotide = head [ x | x <- xs, (not . isNucleotide) x]

        
        
