module School (School, add, empty, grade, sorted) where

import Data.List ( sort, sortBy, groupBy )

type Grade    = Int
type Name     = String
type Student  = (Grade, Name)
type Students = [Student]

newtype School = School Students

add :: Grade -> Name -> School -> School
add grade name (School students) = School ((grade, name) : students)

empty :: School
empty = School []

grade :: Grade -> School -> [Name]
grade grade (School students) = sort [ snd student | student <- students, fst student == grade ]

sorted :: School -> [(Grade, [Name])]
sorted (School students) = map conv . group' . sortBy ordGrade . sortBy ordName $ students
    where
        mapTuple :: (a -> b) -> (a, a) -> (b, b)
        mapTuple f (a1, a2) = (f a1, f a2)

        ordName :: Student -> Student -> Ordering
        ordName = curry $ uncurry compare . mapTuple snd

        ordGrade :: Student -> Student -> Ordering
        ordGrade = curry $ uncurry compare . mapTuple fst

        group' :: Students -> [Students]
        group' = groupBy (\x y -> fst x == fst y)

        conv :: Students -> (Grade, [Name])
        conv xs = (fst . head $ xs, map snd xs)

