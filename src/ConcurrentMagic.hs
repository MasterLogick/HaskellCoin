module ConcurrentMagic where

import Control.Concurrent.MVar


type Locker = MVar ()

newLocker :: IO Locker
newLocker = newEmptyMVar

waitAny :: Locker -> IO ()
waitAny locker = readMVar locker

notifyAll :: Locker -> IO ()
notifyAll locker = do
    tryPutMVar locker ()
    tryTakeMVar locker
    return ()
