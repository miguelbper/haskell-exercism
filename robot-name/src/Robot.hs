module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State ( StateT(..), runState, put, get )
import Data.Set ( Set, empty, member, insert )
import Data.IORef ( IORef, readIORef, newIORef, writeIORef )
import System.Random ( Random(randomR), getStdRandom )
import Control.Monad.Trans ( lift )

type Name = String

newtype Robot    = Robot    { name  :: IORef Name } deriving (Eq)
newtype RunState = RunState { names :: Set   Name } deriving (Eq)

robotName :: Robot -> IO Name
robotName = readIORef . name

initialState :: RunState
initialState = RunState empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
    ref <- lift $ newIORef ""
    let robot = Robot ref
    resetName robot
    pure robot

resetName :: Robot -> StateT RunState IO ()
resetName robot@(Robot ref) = do
    newName             <- lift randomName
    (RunState namePool) <- get
    if member newName namePool
        then resetName robot
        else do
            put . RunState $ insert newName namePool
            lift $ writeIORef ref newName

randomName :: IO String
randomName = do
    a1 <- getStdRandom $ randomR ('A', 'Z')
    a2 <- getStdRandom $ randomR ('A', 'Z')
    n1 <- getStdRandom $ randomR ('0', '9')
    n2 <- getStdRandom $ randomR ('0', '9')
    n3 <- getStdRandom $ randomR ('0', '9')
    return [a1, a2, n1, n2, n3]