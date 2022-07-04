{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module AccountBalance where

import Control.Concurrent.MVar

import MinerState
import TBlock

-- | block creation fee
reward :: Amount
reward = 10

-- | Checks if id_sender in this transaction
getTransaction :: SenderHash -> Transaction -> TransList
getTransaction id_sender (Transaction senderHash recvHash amount time sign)
 | id_sender == senderHash = [Transaction senderHash recvHash amount time sign]
 | id_sender == recvHash = [Transaction senderHash recvHash amount time sign]
 | otherwise = []

-- | Gets transaction of user from transaction list
getTransactionsBlock :: SenderHash -> TransList -> TransList
getTransactionsBlock _ [] = []
getTransactionsBlock id_sender (transition: transList) =
  getTransaction id_sender transition ++ getTransactionsBlock id_sender transList

-- | Gets transactions' list of user with usage of blocks' array.
getListTransactions :: SenderHash -> [Block] -> TransList
getListTransactions _ [] = []
getListTransactions id_sender ((Block prevHash minerHash nonce transCount transList): xs) =
  getTransactionsBlock id_sender transList ++ getListTransactions id_sender xs

-- | Gets user balance from transaction list
getBalanceTransactions :: SenderHash -> TransList -> Amount
getBalanceTransactions _ [] = 0
getBalanceTransactions id_sender ((Transaction senderHash recvHash amount _ _): xs) =
  amount' + getBalanceTransactions id_sender xs
 where
    amount'
      | id_sender == senderHash = -amount
      | id_sender == recvHash = amount
      | otherwise = 0

-- | Gets amount of mined blocks.
getAmountMinedBlocks :: SenderHash -> [Block] -> Amount
getAmountMinedBlocks _ [] = 0
getAmountMinedBlocks id_sender ((Block prevHash minerHash nonce transCount transList): xs)
  | minerHash == id_sender = 1 + getAmountMinedBlocks id_sender xs
  | otherwise = getAmountMinedBlocks id_sender xs

-- | Gets user's balance.
getBalance :: SenderHash -> [Block] -> TransList -> Amount
getBalance id_sender blocks transList = (getAmountMinedBlocks id_sender blocks) * reward + (getBalanceTransactions id_sender transList)

-- | Gets transaction list of user.
prettyTransactions :: SenderHash -> TransList -> String
prettyTransactions _ [] = ""
prettyTransactions id_sender ((Transaction senderHash recvHash amount time sign) : xs) = 
  result ++ (prettyTransactions id_sender xs)
  where
    result
      | id_sender == senderHash = (show senderHash) ++ " -> " ++ (show recvHash) ++ " | " ++ (show time) ++ " | " ++ (show amount)  ++ "\n"
      | id_sender == recvHash = (show recvHash) ++ " <- " ++ (show senderHash)++ " | " ++ (show time) ++ " | " ++ (show amount) ++ "\n"
      | otherwise = ""

-- | Gets transactions' balance of user.
userBalance :: SenderHash -> Handler
userBalance id_sender stateRef = do
    miner <- readMVar stateRef
    let pendingTrans = pendingTransactions miner
    let transactions = (getListTransactions id_sender (blocks miner)) ++ (getTransactionsBlock id_sender pendingTrans)
    putStrLn("From:                                       To:                                        Date:                            Amount:")
    putStrLn (prettyTransactions id_sender transactions)
    putStrLn ("Balance: " ++ (show $ getBalance id_sender (blocks miner) transactions))
    return ()