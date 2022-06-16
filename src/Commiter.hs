module Commiter where

import MinerState
import TBlock

commitTransaction :: Transaction -> MinerState -> MinerState
commitTransaction newTransaction state = state{pendingTransactions = (pendingTransactions state ++ [newTransaction])}