module MinerState where

import TBlock

data MinerState = MinerState {
    blocks :: [Block],
    pendingTransactions :: [Transaction]
}

type Handler = MinerState -> IO (Maybe MinerState)
