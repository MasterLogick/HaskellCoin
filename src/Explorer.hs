{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Explorer where

import Control.Concurrent.MVar

import MinerState
import TBlock

-- | Gets transactions' description.
getEnumeratedTransList :: Integer -> [Transaction] -> String
getEnumeratedTransList _ [] = ""
getEnumeratedTransList num ((Transaction senderHash recvHash amount time _): xs) 
    = "~~~~~~~~~~~~~~~\nTransaction number: " ++ show num 
    ++ "\nSender Hash: " ++ show senderHash 
    ++ "\nRecv Hash: "   ++ show recvHash 
    ++ "\nAmount: "      ++ show amount 
    ++ "\nTime: "        ++ show time
    ++ "\n"              ++ getEnumeratedTransList (num + 1) xs

-- | Gets a list of transactions.
getTransList :: [Transaction] -> String
getTransList = getEnumeratedTransList 1

-- | Gets information about block.
getBlockInfo :: Block -> String
getBlockInfo (Block prevHash minerHash nonce transCount transList) 
    = "Prev Hash: "         ++ show prevHash 
    ++ "\nMiner Hash: "     ++ show minerHash 
    ++ "\nNonce: "          ++ show nonce
    ++ "\nTrans Count: "    ++ show transCount 
    ++ "\nTransactions: \n" ++ getTransList transList

-- | Gets information about list of blocks.
sumBlocksInfo :: [Block] -> String
sumBlocksInfo [] = "\n"
sumBlocksInfo (x: xs)
    = getBlockInfo x ++ "##################\n"
    ++ sumBlocksInfo xs 

-- | Prints all blocks in console.
exploreNetwork :: Handler
exploreNetwork stateRef = do
    miner <- readMVar stateRef
    putStrLn $ sumBlocksInfo $ blocks miner
    return ()