module Commiter where

import MinerState
import TBlock

commitTransaction :: MinerState -> Transaction -> MinerState
commitTransaction (MinerState blocks transactions) newTransaction = MinerState blocks (newTransaction:transactions)