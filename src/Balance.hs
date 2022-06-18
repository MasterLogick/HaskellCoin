module Balance where

import Control.Concurrent.MVar

import MinerState
import TBlock


getTransactionBalance :: SenderHash -> Transaction -> Amount
getTransactionBalance id_sender (Transaction senderHash recvHash amount _) 
 | id_sender == senderHash = (-amount)
 | id_sender == recvHash = amount
 | otherwise = 0

getBalanceBlock :: SenderHash -> TransList -> Amount
getBalanceBlock _ [] = 0
getBalanceBlock id_sender (transition: transList) = getTransactionBalance id_sender transition


getBalance :: SenderHash -> [Block] -> Amount
getBalance _ [] = 0
getBalance id_sender ((Block prevHash minerHash nonce transCount transList): xs) = getBalanceBlock id_sender transList + getBalance id_sender xs

userBalance :: SenderHash -> Handler
userBalance id_sender stateRef = do
    miner <- readMVar stateRef
    putStrLn (show $ getBalance id_sender (blocks miner))
    return ()