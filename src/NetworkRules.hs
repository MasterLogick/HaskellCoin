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
    userBalance = getBalance senderHash minerBlocks (userTransactions ++ pendingTrans)

data Judgement = Accept | AlreadyPresent | BranchDivergence -- | BadSignature

judgeBlock :: MinerState -> Block -> Judgement
judgeBlock minerState newBlock =
    if (elem newBlock (blocks minerState)) then
        AlreadyPresent
    else
        if getBlockHash (getNewestBlock minerState) == (bPrevHash newBlock) then
            Accept
        else
            BranchDivergence

mergeBranches :: [Block] -> [Block] -> Either [Block] [Block]
mergeBranches [] presentPart = Right presentPart
mergeBranches incomingPart [] = Left incomingPart
mergeBranches incoming present =
    if not (null incomingPart) then
        mergeUnsafe 
    else
        Right present
    where
        (incomingPart,_) = break (\x -> elem x present) incoming
        mergeUnsafe :: Either [Block] [Block]
        mergeUnsafe =
            if (length localDivergedPart) > (length incomingPart) then
                Right present
            else if (length localDivergedPart) < (length incomingPart) then
                 Left (incomingPart ++ commonPart)
            else if (getBlockHash (head incomingPart)) < (getBlockHash (head localDivergedPart)) then
                Left (incomingPart ++ commonPart)
            else (Right present)
            where
                lastCommonBlockHash = bPrevHash (head (reverse incomingPart))
                (localDivergedPart, commonPart) = break (\x -> (getBlockHash x) == lastCommonBlockHash) present