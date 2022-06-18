module AccountBalance where

import Control.Concurrent.MVar

import MinerState
import TBlock


-- | Getting of transaction's balance.
getTransactionBalance :: SenderHash -> Transaction -> Amount
getTransactionBalance id_sender (Transaction senderHash recvHash amount _) 
 | id_sender == senderHash = (-amount)
 | id_sender == recvHash = amount
 | otherwise = 0

 -- | Getting of block's balance.
getBalanceBlock :: SenderHash -> TransList -> Amount
getBalanceBlock _ [] = 0
getBalanceBlock id_sender (transition: transList) = getTransactionBalance id_sender transition + getBalanceBlock id_sender transList

-- | Getting transactions' balance of user with usage of blocks' array.
getBalance :: SenderHash -> [Block] -> Amount
getBalance _ [] = 0
getBalance id_sender ((Block prevHash minerHash nonce transCount transList): xs) = getBalanceBlock id_sender transList + getBalance id_sender xs

-- | Getting transactions' balance of user.
userBalance :: SenderHash -> Handler
userBalance id_sender stateRef = do
    miner <- readMVar stateRef
    putStrLn (show $ getBalance id_sender (blocks miner))
    return ()