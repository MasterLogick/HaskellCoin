{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Commiter where

import Control.Concurrent.MVar
import Data.Binary

import Data.ByteString
import CryptoMagic
import MinerState
import TBlock
import NetworkRules
import NetworkMagic

-- | Adds new transaction to pending block.
commitTransaction :: TransactionCandidate -> Handler
commitTransaction candidate@(TransactionCandidate sender receiver amount time) stateRef = do
    miner <- readMVar stateRef
    if amount <= 0 
        then
            putStrLn "Print invalid amount."
    else
      do
        let enoughCoins = checkEnoughCoins miner sender amount
        case enoughCoins of
            True -> do
                modifyMVar stateRef (\miner -> do
                        let private = getKeyFromPair Private $ keyPair miner
                        let public = getKeyFromPair Public $ keyPair miner
                        signature <- signMsg private (toStrict $ encode candidate)
                        let newTransaction = Transaction sender receiver amount time (public, signature)
                        propagateLastPendingTransactionToNet stateRef
                        return (miner{pendingTransactions = pendingTransactions miner ++ [newTransaction]}, ())
                        )
                putStrLn  "Transaction is added to pending block and signed."
            False -> 
                    putStrLn  "Not enough coins to make this transaction."
