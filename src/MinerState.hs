module MinerState where

import TBlock

data MinerState = MinerState [Block] [Transaction]
