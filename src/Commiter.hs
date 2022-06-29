module Commiter where

import Control.Concurrent.MVar
import  Data.Binary

import Data.ByteString
import CryptoMagic
import MinerState
import TBlock

-- | Adding new transaction to pending block.
commitTransaction :: TransactionCandidate -> Handler
commitTransaction candidate@(TransactionCandidate sender receiver amount) stateRef = do
    --let private = getKeyFromPair Private $ keyPair miner
    modifyMVar stateRef (\miner -> do
        let private = getKeyFromPair Private $ keyPair miner
        let public = getKeyFromPair Public $ keyPair miner
        signature <- signMsg private (toStrict $ encode candidate)
        let newTransaction = Transaction sender receiver amount (public, signature)
        return (miner{pendingTransactions = pendingTransactions miner ++ [newTransaction]}, ())
        )
    putStrLn  "Transaction is added to pending block and signed."