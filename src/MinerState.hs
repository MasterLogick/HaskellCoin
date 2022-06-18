module MinerState where

import Control.Concurrent.MVar
import Network.Socket
import qualified Data.ByteString.Lazy as LB

import TBlock

-- | data MinerState storers information about blocks, pendings transactions,
-- | network and boole flag on exit
data MinerState = MinerState {
    blocks :: [Block],
    pendingTransactions :: [Transaction],
    network :: [NetUser],
    shouldExit :: Bool
}

-- | the type of commands available to enter into the console
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