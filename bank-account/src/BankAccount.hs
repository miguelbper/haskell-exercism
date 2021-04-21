module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Monad ( void )
import Control.Arrow ( (&&&) )
import Control.Concurrent ( MVar, newMVar, readMVar, swapMVar, modifyMVar )

type BankAccount = MVar (Maybe Integer)

openAccount :: IO BankAccount
openAccount = newMVar $ Just 0

closeAccount :: BankAccount -> IO ()
closeAccount = void . flip swapMVar Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance = readMVar

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = modifyMVar account (return . ( id &&& id ) . fmap (amount +))
