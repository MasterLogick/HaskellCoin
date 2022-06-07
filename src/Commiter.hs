module Commiter where

import MinerState

commitTransaction :: MinerState -> Transaction -> MinerState
commitTransaction (MinerState blocks transactions) newTransaction = MinerState blocks newTransaction:transactions