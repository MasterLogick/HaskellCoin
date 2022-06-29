module Explorer where

import Control.Concurrent.MVar

import MinerState
import TBlock

-- | Getting of transactions' description.
getEnumeratedTransList :: Integer -> [Transaction] -> String
getEnumeratedTransList _ [] = ""
getEnumeratedTransList num ((Transaction senderHash recvHash amount _signature): xs) 
    = "~~~~~~~~~~~~~~~\nTransaction number: " ++ show num 
    ++ "\nSender Hash: " ++ show senderHash 
    ++ "\nRecv Hash: "   ++ show recvHash 
    ++ "\nAmount: "      ++ show amount 
    ++ "\n"              ++ getEnumeratedTransList (num + 1) xs

-- | Getting a list of transactions.
getTransList :: [Transaction] -> String
getTransList transList = getEnumeratedTransList 1 transList

-- | Getting information about block.
getBlockInfo :: Block -> String
getBlockInfo (Block prevHash minerHash nonce transCount transList) 
    = "Prev Hash: "         ++ show prevHash 
    ++ "\nMiner Hash: "     ++ show minerHash 
    ++ "\nNonce: "          ++ show nonce
    ++ "\nTrans Count: "    ++ show transCount 
    ++ "\nTransactions: \n" ++ getTransList transList

-- | Getting information about list of blocks.
sumBlocksInfo :: [Block] -> String
sumBlocksInfo [] = "\n"
sumBlocksInfo (x: xs) 
    = getBlockInfo x ++ "##################\n" 
    ++ sumBlocksInfo xs 

-- | Printing of all blocks in console.
exploreNetwork :: Handler
exploreNetwork stateRef = do
    miner <- readMVar stateRef
    putStrLn $ sumBlocksInfo $ blocks miner
    return ()