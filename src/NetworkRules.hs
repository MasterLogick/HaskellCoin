module NetworkRules where

import Data.Binary
import Data.ByteString hiding (head, dropWhile, elem, map, null, break, length, reverse)
import MinerState
import TBlock
import AccountBalance
import CryptoMagic

data Judgement = Accept | AlreadyPresent | BranchDivergence -- | BadSignature

checkEnoughCoins :: MinerState -> SenderHash -> Amount -> Bool
checkEnoughCoins minerState senderHash amount
  | userBalance >= amount = True
  | otherwise = False
  where
    pendingTrans = pendingTransactions minerState
    minerBlocks = blocks minerState
    userTransactions = getListTransactions senderHash minerBlocks
    userBalance = getBalance senderHash minerBlocks (userTransactions ++ pendingTrans)

genNonces :: Block -> [Block]
genNonces (Block prevHash minerId _ trans transList) = blocks
    where
        blocks = map compueWithNonce [0,1..]
        compueWithNonce :: Nonce -> Block
        compueWithNonce nonce =
            Block prevHash minerId nonce trans transList

mineBlock :: Block -> Block
mineBlock block = head $ dropWhile hashed (genNonces block)
    where
        hashed :: Block -> Bool
        hashed block = hashFunc (toStrict $ encode block) < ruleHash

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