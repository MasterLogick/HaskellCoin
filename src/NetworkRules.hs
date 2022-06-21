module NetworkRules where

import MinerState
import TBlock

data Judgement = Accept | Decline | BranchDivergence

judgeBlock :: MinerState -> Block -> Judgement
judgeBlock minerState newBlock =
    if (elem newBlock (blocks minerState)) then
        Decline
    else
        if getBlockHash (getNewestBlock minerState) == (bPrevHash newBlock) then
            Accept
        else
            BranchDivergence