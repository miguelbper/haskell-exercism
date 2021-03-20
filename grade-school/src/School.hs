module School (School, add, empty, grade, sorted) where

import Data.List ( sort, groupBy )

type Grade     = Int
type Name      = String
type Student   = (Grade, Name)
newtype School = School [Student] deriving (Eq, Ord)

add :: Grade -> Name -> School -> School
add grade name (School students) = School ((grade, name) : students)

empty :: School
empty = School []

grade :: Grade -> School -> [Name]
grade grade (School students) = sort [ snd student | student <- students, fst student == grade ]

sorted :: School -> [(Grade, [Name])]
sorted (School students) = map convert . group' . sort $ students
    where
        group' :: [Student] -> [[Student]]
        group' = groupBy (\x y -> fst x == fst y)

        convert :: [Student] -> (Grade, [Name])
        convert xs = (fst . head $ xs, map snd xs)

