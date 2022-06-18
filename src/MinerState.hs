module MinerState where

import Control.Concurrent.MVar
import Network.Socket

import TBlock

-- | data MinerState storers information about blocks, pendings transactions,
-- | network and boole flag on exit
data MinerState = MinerState {
    blocks :: [Block],
    pendingTransactions :: [Transaction],
    network :: [Socket],
    shouldExit :: Bool
}

-- | the type of commands available to enter into the console
type Handler = MVar MinerState -> IO ()
