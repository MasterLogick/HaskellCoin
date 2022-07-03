module NetworkRules where

import Data.Binary
import Data.ByteString hiding (head, dropWhile, elem, map, null, break, length, reverse)
import Crypto.PubKey.ECDSA 
import MinerState
import TBlock
import AccountBalance
import CryptoMagic
import Control.Exception
import Crypto.Hash
import Crypto.ECC
import Crypto.Hash
import Crypto.Error
import Crypto.Random.Types
import Crypto.PubKey.ECDSA as ECDSA

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

mineBlock :: Block -> IO Block
mineBlock block = evaluate (head (dropWhile (not . validateBlockNonce) (genNonces block)))

validateBlockNonce :: Block -> Bool
validateBlockNonce block = case show (hashFunc (toStrict $ encode block)) of
    '0':'0':'0':'0':'0':_ -> True
    _ -> False

-- | Validates transaction signature
validateTransaction :: Transaction -> Bool
validateTransaction trans@(Transaction _ _ _ (pubkey, signature)) =
    verifyStringMsg pubkey (toStrict $ encode trans) signature

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