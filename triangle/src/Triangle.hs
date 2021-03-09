module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
    | isIllegal     = Illegal
    | isEquilateral = Equilateral
    | isIsosceles   = Isosceles
    | isScalene     = Scalene
    | otherwise     = Illegal
    where
        isIllegal     = not (a > 0 && b > 0 && c > 0 && a + b > c && b + c > a && c + a > b )
        nEqualities   = length (filter (==True) [a == b, a == c, b == c])
        isEquilateral = nEqualities == 3
        isIsosceles   = nEqualities == 1
        isScalene     = nEqualities == 0
