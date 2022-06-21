module MinerState where

import Control.Concurrent.MVar
import Network.Socket
import Data.Maybe
import qualified Data.ByteString.Lazy as LB

import TBlock
import CryptoMagic

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

type InBuffer = LB.ByteString

data NetUser = NetUser {
    nuSocket :: Socket,
    nuBuffer :: MVar InBuffer,
    nuService :: MVar Bool
}

newNetUser :: Socket -> IO NetUser
newNetUser sock = do
    buffer <- newMVar LB.empty
    service <- newMVar False
    return (NetUser sock buffer service)

getNewestBlock :: MinerState -> Block
getNewestBlock minerState = fromMaybe fallbackBlock (listToMaybe (blocks minerState))

getBlockByHash :: MinerState -> BlockHash -> Maybe Block
getBlockByHash minerState hash = listToMaybe (filter filterRule (blocks minerState))
    where
        filterRule block = (getBlockHash block) == hash