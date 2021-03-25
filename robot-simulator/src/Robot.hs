module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot { br :: Bearing,
                     x  :: Integer,
                     y  :: Integer }

bearing :: Robot -> Bearing
bearing = br

coordinates :: Robot -> (Integer, Integer)
coordinates robot = (x robot, y robot)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction (x,y) = Robot direction x y 

move :: Robot -> String -> Robot
move = foldl changeState
    where 
        changeState :: Robot -> Char -> Robot
        changeState rb c
            | c == 'A' = advance   rb
            | c == 'L' = turnLeft  rb
            | c == 'R' = turnRight rb
        
        advance :: Robot -> Robot
        advance (Robot br a b)
            | br == North = Robot North a (b + 1)
            | br == East  = Robot East  (a + 1) b
            | br == South = Robot South a (b - 1)
            | br == West  = Robot West  (a - 1) b

        turnLeft :: Robot -> Robot
        turnLeft (Robot br a b)
            | br == North = Robot West      a b
            | otherwise   = Robot (pred br) a b

        turnRight :: Robot -> Robot
        turnRight (Robot br a b)
            | br == West  = Robot North a b
            | otherwise   = Robot (succ br) a b