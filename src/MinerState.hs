module MinerState where

import TBlock

data MinerState = MinerState [Block] [Transaction]

type Handler = MinerState -> (IO (), Maybe MinerState)
