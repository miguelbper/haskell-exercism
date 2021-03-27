module ProteinTranslation(proteins) where

type RNA = String
type Codon = String
type Protein = String

chunks :: Int -> [a] -> [[a]]
chunks n xs
    | length xs <= n = [xs]
    | otherwise      = take n xs : chunks n (drop n xs)

codon :: RNA -> [Codon]
codon = chunks 3

protein :: Codon -> Maybe Protein
protein xs
    | xs `elem` ["AUG"]                      = Just "Methionine"
    | xs `elem` ["UUU", "UUC"]               = Just "Phenylalanine"
    | xs `elem` ["UUA", "UUG"]               = Just "Leucine"
    | xs `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
    | xs `elem` ["UAU", "UAC"]               = Just "Tyrosine"
    | xs `elem` ["UGU", "UGC"]               = Just "Cysteine"
    | xs `elem` ["UGG"]                      = Just "Tryptophan"
    | xs `elem` ["UAA", "UAG", "UGA"]        = Just "STOP"
    | otherwise                          = Nothing

mapLM :: [Maybe a] -> Maybe [a]
mapLM = foldr f (Just [])
    where
        f :: Maybe a -> Maybe [a] -> Maybe [a]
        f _ Nothing = Nothing
        f Nothing _ = Nothing
        f (Just x) (Just xs) = Just (x:xs) 

proteins :: RNA -> Maybe [Protein]
proteins = fmap (takeWhile (/= "STOP")) . mapLM . fmap protein . codon
