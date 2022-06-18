module MinerState where

import Control.Concurrent.MVar
import Network.Socket
import qualified Data.ByteString.Lazy as LB

import TBlock

data MinerState = MinerState {
    blocks :: [Block],
    pendingTransactions :: [Transaction],
    network :: [NetUser],
    shouldExit :: Bool
}

type Handler = MVar MinerState -> IO ()

data NetUser = NetUser {
    nuSocket :: Socket,
    nuState :: MVar NetUserState
}

newNetUser :: Socket -> IO NetUser
newNetUser sock = do
    state <- newMVar (NetUserState LB.empty False)
    return (NetUser sock state)

data NetUserState = NetUserState {
    nuBuffer :: LB.ByteString,
    nuService :: Bool
}