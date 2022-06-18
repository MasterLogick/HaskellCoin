module Commiter where

import Control.Concurrent.MVar

import MinerState
import TBlock

-- | Adding new transaction to pending block.
commitTransaction :: Transaction -> Handler
commitTransaction newTransaction stateRef = do
    modifyMVar stateRef (\miner -> return (miner{pendingTransactions = (pendingTransactions miner ++ [newTransaction])}, ())
        )
    putStrLn  "Transaction is added to pending block."