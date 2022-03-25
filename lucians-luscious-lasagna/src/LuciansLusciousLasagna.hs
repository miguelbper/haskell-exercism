module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

expectedMinutesInOven = 40

preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes = (*) 2

elapsedTimeInMinutes :: Int -> Int -> Int
elapsedTimeInMinutes layers timeInOven = timeInOven + preparationTimeInMinutes layers