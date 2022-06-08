module Commiter where

import MinerState
import TBlock

commitTransaction :: Transaction -> MinerState -> MinerState
commitTransaction newTransaction (MinerState blocks transactions)  = MinerState blocks (transactions ++ [newTransaction])