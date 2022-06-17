module Sender where

import Crypto.Hash
import qualified Data.ByteString as DBY
import Control.Concurrent.MVar
import Data.Binary
import Data.ByteArray
import Data.Maybe

import MinerState
import TBlock
import CryptoMagic
import NetworkMagic

getLast :: [a] -> Maybe a
getLast [] = Nothing
getLast [e] = Just e
getLast (e:es) = getLast es

blockHash :: Maybe Block -> BlockHash
blockHash Nothing = hashFunc $ DBY.toStrict $ Data.Binary.encode (0 :: Int) -- hash of (0 :: Int)
blockHash (Just block)
     = hashFunc (DBY.toStrict $ encode block)

buildAndSendToNet :: Handler
buildAndSendToNet stateRef = do
    modifyMVar stateRef (\miner -> do
        let blockchain = blocks miner
        let pending = pendingTransactions miner
        let hashedPrev :: BlockHash; hashedPrev = blockHash (listToMaybe $ Prelude.reverse $ blockchain)
        let newBlock :: Block; newBlock = Block hashedPrev (blockHash Nothing) 0 (Prelude.length pending) pending
        propagateBlockToNet stateRef
        let newChain :: [Block]; newChain = newBlock : blockchain
        return (miner{blocks = newChain, pendingTransactions = []}, ())
        )
    putStrLn "Block is built."