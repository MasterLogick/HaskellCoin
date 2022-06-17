module MinerState where

import Control.Concurrent.MVar
import Network.Socket

import TBlock

data MinerState = MinerState {
    blocks :: [Block],
    pendingTransactions :: [Transaction],
    network :: [Socket],
    shouldExit :: Bool
}

type Handler = MVar MinerState -> IO ()
