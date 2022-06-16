module Commiter where

import MinerState
import TBlock

commitTransaction :: Transaction -> Handler
commitTransaction newTransaction state = do
    putStrLn  "Transaction is added to pending block."
    return $ Just state {pendingTransactions = (pendingTransactions state ++ [newTransaction])}