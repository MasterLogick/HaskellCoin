module NetworkRules where

import Data.Binary
import Data.ByteString hiding (head, dropWhile, elem, map, null, break, length, reverse)
import MinerState
import TBlock
import AccountBalance
import CryptoMagic
import Control.Exception

import qualified Data.List as DL
-- import Data.List (elemIndex)

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

revard :: Amount
revard = 10

getValue :: Eq a => a -> [(a, b)] -> Maybe b
getValue key dict = lookup key dict

setValue :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
setValue key newVal dict = newDict
  where
    indexElement = DL.elemIndex key (map fst dict)

    newDict = case indexElement of
                Just index -> result
                  where
                    (prefix, _: suffix) = DL.splitAt index dict
                    result = prefix ++ ((key, newVal) : suffix)
                Nothing -> (key, newVal) : dict

type BalanceState = [(SenderHash, Amount)]
data SystemState = SystemState [(SenderHash, Amount)] TransList

-- проверка сигнатур транзакций
-- проверка что транзакция уникальна (ее не было раньше)
-- достаточно ли денег для транзакции [Block] -> Transaction -> bool
validateWholeChain :: [Block] -> TransList -> Bool
validateWholeChain blocks pendingTrans = result
  where 
    result = validateChain (reverse blocks) [] pendingTrans

validateChain :: [Block] -> [(SenderHash, Amount)] -> TransList -> Bool
validateChain blocks uBalance pendingTrans = case validateBlocks blocks newState of 
    Nothing -> False
    Just state@(SystemState usersBalance transes) -> 
      case validateTransactions pendingTrans state of
        Nothing -> False
        Just _ -> True
  where
    newState = Just (SystemState uBalance [])

validateBlocks :: [Block] -> Maybe SystemState -> Maybe SystemState
validateBlocks _ Nothing = Nothing
validateBlocks [] state = state
validateBlocks (block: xs) (Just state) = case validateConnection block xs of
  False -> Nothing
  True -> case newState of
      Nothing -> Nothing
      Just _ -> validateBlocks xs newState
    where
      newState = validateBlock block state

validateConnection :: Block -> [Block] -> Bool
validateConnection block1 [] = True
validateConnection block1 (block2: xs) = if getBlockHash block1 == (bPrevHash block2) then True else False

validateBlock :: Block -> SystemState -> Maybe SystemState
validateBlock block state@(SystemState usersBalance transes) = newState
  where
    tmpState = validateTransactions (bTransList block) state
    newState = case tmpState of
      Nothing -> Nothing
      Just (SystemState usersBalance newTranses) -> Just (SystemState newUsersBalance newTranses)
        where 
          newUsersBalance = case lookup (bMinerHash block) usersBalance of
            Nothing -> setValue (bMinerHash block) revard usersBalance 
            Just value -> setValue (bMinerHash block) (revard + value) usersBalance 
  
validateTransactions :: TransList -> SystemState -> Maybe SystemState
validateTransactions [] state = Just state
validateTransactions (transaction: xs) state = 
  case validateTransaction transaction state of
      Nothing -> Nothing                                               
      Just newState' -> validateTransactions xs newState'

validateTransaction :: Transaction -> SystemState -> Maybe SystemState
validateTransaction transaction@(Transaction senderHash recvHash amount _ hSignature) (SystemState usersBalance transes) = result
  where
    result = case validateTransactionSignature transaction of
      False -> Nothing
      True -> case DL.find (\x -> x == transaction) transes of -- check is transaction in list of transactions
        Just _ -> Nothing
        Nothing -> case lookup senderHash usersBalance of
          Nothing -> Nothing
          Just senderUserBalance -> if senderUserBalance < amount then Nothing else (Just (SystemState newUsersBalance newTranses))
            where
              tmpUsersBalance = setValue senderHash (senderUserBalance - amount) usersBalance -- change balance of sender
              newUsersBalance = case lookup recvHash tmpUsersBalance of -- change balance of recv
                Nothing -> setValue recvHash amount tmpUsersBalance
                Just recvUserBalance -> setValue recvHash (amount + recvUserBalance) tmpUsersBalance
              newTranses = transaction : transes

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
validateTransactionSignature :: Transaction -> Bool
validateTransactionSignature trans@(Transaction _ _ _ _ (pubkey, signature)) =
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
