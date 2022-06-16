module Explorer where

import MinerState
import TBlock


-- data Transaction = Transaction SenderHash RecvHash Amount


getEnumeratedTransList :: Integer -> [Transaction] -> String
getEnumeratedTransList _ [] = ""
getEnumeratedTransList num ((Transaction senderHash recvHash amount _signature): xs) = "~~~~~~~~~~~~~~~\nTransaction number: " ++ (show num) ++ "\nSender Hash: " ++ (show senderHash) ++ "\nRecv Hash: " ++ (show recvHash) ++ "\nAmount: " ++ (show amount) ++ "\n" ++ (getEnumeratedTransList (num + 1) xs)

getTransList :: [Transaction] -> String
getTransList transList = getEnumeratedTransList 1 transList

getBlockInfo :: Block -> String
getBlockInfo (Block prevHash minerHash nonce transCount transList) = "Prev Hash: " ++ (show prevHash) ++ "\nMiner Hash: " ++ (show minerHash) ++ "\nNonce: " ++ (show nonce) ++ "\nTrans Count: " ++ (show transCount) ++ "\nTransactions: \n" ++ (getTransList transList)

sumBlocksInfo :: [Block] -> String
sumBlocksInfo [] = "\n"
sumBlocksInfo (x: xs) = (getBlockInfo x) ++ "##################\n" ++ sumBlocksInfo xs 

exploreNetwork :: Handler
exploreNetwork state = (putStrLn $ sumBlocksInfo $ blocks $ state, Just state)