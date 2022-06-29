module NetworkRules where

import MinerState
import TBlock
import AccountBalance

checkEnoughCoins :: MinerState -> SenderHash -> Amount -> Bool
checkEnoughCoins minerState senderHash amount
  | userBalance >= amount = True
  | otherwise = False
  where
    pendingTrans = pendingTransactions minerState
    minerBlocks = blocks minerState
    userTransactions = getListTransactions senderHash minerBlocks
    userBalance = getBalance senderHash (userTransactions ++ pendingTrans)

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