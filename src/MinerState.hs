module MinerState where

import Control.Concurrent.MVar

import TBlock

data MinerState = MinerState {
    blocks :: [Block],
    pendingTransactions :: [Transaction],
    shouldExit :: Bool
}

type Handler = MVar MinerState -> IO ()
