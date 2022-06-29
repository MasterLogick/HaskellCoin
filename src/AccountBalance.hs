module AccountBalance where

import Control.Concurrent.MVar

import MinerState
import TBlock


-- | Checks if id_sender in this transaction
getTransaction :: SenderHash -> Transaction -> TransList
getTransaction id_sender (Transaction senderHash recvHash amount sign)
 | id_sender == senderHash = [Transaction senderHash recvHash amount sign]
 | id_sender == recvHash = [Transaction senderHash recvHash amount sign]
 | otherwise = []

-- | Getting transaction of user from transaction list
getTransactionsBlock :: SenderHash -> TransList -> TransList
getTransactionsBlock _ [] = []
getTransactionsBlock id_sender (transition: transList) =
  getTransaction id_sender transition ++ getTransactionsBlock id_sender transList

-- | Getting transactions' list of user with usage of blocks' array.
getListTransactions :: SenderHash -> [Block] -> TransList
getListTransactions _ [] = []
getListTransactions id_sender ((Block prevHash minerHash nonce transCount transList): xs) =
  getTransactionsBlock id_sender transList ++ getListTransactions id_sender xs

-- | Get user balance from transaction list
getBalanceTransactions :: SenderHash -> TransList -> Amount
getBalanceTransactions _ [] = 0
getBalanceTransactions id_sender ((Transaction senderHash recvHash amount _): xs) =
  amount' + getBalanceTransactions id_sender xs
 where
    amount'
      | id_sender == senderHash = -amount
      | id_sender == recvHash = amount
      | otherwise = 0


getAmountMinedBlocks :: SenderHash -> [Block] -> Amount
getAmountMinedBlocks _ [] = 0
getAmountMinedBlocks id_sender ((Block prevHash minerHash nonce transCount transList): xs) 
  | minerHash == id_sender = 1 + getAmountMinedBlocks id_sender xs
  | otherwise = getAmountMinedBlocks id_sender xs

getBalance :: SenderHash -> [Block] -> TransList -> Amount
getBalance id_sender blocks transList = (getAmountMinedBlocks id_sender blocks) * 10 + (getBalanceTransactions id_sender transList)

prettyTransactions :: SenderHash -> TransList -> String
prettyTransactions _ [] = ""
prettyTransactions id_sender ((Transaction senderHash recvHash amount sign) : xs) = 
  result ++ (prettyTransactions id_sender xs)
  where
    result
      | id_sender == senderHash = (show senderHash) ++ " -> " ++ (show recvHash) ++ " | " ++ (show amount) ++ "\n"
      | id_sender == recvHash = (show recvHash) ++ " <- " ++ (show senderHash) ++ " | " ++ (show amount) ++ "\n"
      | otherwise = ""


-- | Getting transactions' balance of user.
userBalance :: SenderHash -> Handler
userBalance id_sender stateRef = do
    miner <- readMVar stateRef
    let pendingTrans = pendingTransactions miner
    let transactions = (getListTransactions id_sender (blocks miner)) ++ (getTransactionsBlock id_sender pendingTrans)
    putStrLn("From:                                       To:                                        Amount:")
    putStrLn (prettyTransactions id_sender transactions)
    putStrLn ("Balance: " ++ (show $ getBalance id_sender (blocks miner) transactions))
    return ()