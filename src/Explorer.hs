module Explorer where

import MinerState
import TBlock


-- data Transaction = Transaction SenderHash RecvHash Amount


getTransList :: [Transaction] -> String
getTransList [] = ""
getTransList ((Transaction senderHash recvHash amount): xs) = "~~~ Sender Hash: " ++ (show senderHash) ++ "\nRecv Hash: " ++ (show recvHash) ++ "\nAmount: " ++ (show amount) ++ "\n" ++ (getTransList xs)

getBlockInfo :: Block -> String
getBlockInfo (Block prevHash minerHash nonce transCount transList) = "Prev Hash: " ++ (show prevHash) ++ "\nMiner Hash: " ++ (show minerHash) ++ "\nNonce: " ++ (show nonce) ++ "\nTrans Count: " ++ (show transCount) ++ "\nTransactions: \n" ++ (getTransList transList)

sumBlocksInfo :: [Block] -> String
sumBlocksInfo [] = "\n"
sumBlocksInfo (x: xs) = (getBlockInfo x) ++ "##################\n" ++ sumBlocksInfo xs 

exploreNetwork :: MinerState -> (String, Maybe MinerState)
exploreNetwork (MinerState blocks trans) = (sumBlocksInfo blocks, Just (MinerState blocks trans))