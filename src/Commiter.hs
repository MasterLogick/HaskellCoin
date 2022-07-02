module Commiter where

import Control.Concurrent.MVar
import  Data.Binary

import Data.ByteString
import CryptoMagic
import MinerState
import TBlock
import NetworkRules

-- | Adds new transaction to pending block.
commitTransaction :: TransactionCandidate -> Handler
commitTransaction candidate@(TransactionCandidate sender receiver amount) stateRef = do
    miner <- readMVar stateRef
    let enoughCoins = checkEnoughCoins miner sender amount
    case enoughCoins of
        True -> modifyMVar stateRef (\miner -> do
                    let private = getKeyFromPair Private $ keyPair miner
                    let public = getKeyFromPair Public $ keyPair miner
                    signature <- signMsg private (toStrict $ encode candidate)
                    putStrLn  "Transaction is added to pending block and signed."
                    let newTransaction = Transaction sender receiver amount (public, signature)
                    return (miner{pendingTransactions = pendingTransactions miner ++ [newTransaction]}, ())
                    )
        False -> 
                putStrLn  "Not enough coins to make this transaction."