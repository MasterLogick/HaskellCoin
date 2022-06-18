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

-- | Function getLast gets the last list item
getLast :: [a] -> Maybe a
getLast [] = Nothing
getLast [e] = Just e
getLast (e:es) = getLast es

-- | function blockHash hashes the block
blockHash :: Maybe Block -> BlockHash
blockHash Nothing = hashFunc $ DBY.toStrict $ Data.Binary.encode (0 :: Int) -- hash of (0 :: Int)
blockHash (Just block)
     = hashFunc (DBY.toStrict $ encode block)

-- | function buildAndSendToNet builds and sends a block into the network
-- | and as a result, the line that the block is built
buildAndSendToNet :: Handler
buildAndSendToNet stateRef = do
    modifyMVar stateRef (\minerState -> do
        let blockchain = blocks minerState
        let pending = pendingTransactions minerState
        let hashedPrev :: BlockHash; hashedPrev = blockHash (listToMaybe $ Prelude.reverse $ blockchain)
        let newBlock :: Block; newBlock = Block hashedPrev (blockHash Nothing) 0 (Prelude.length pending) pending
        propagateLastBlockToNet stateRef
        let newChain :: [Block]; newChain = newBlock : blockchain
        return (minerState{blocks = newChain, pendingTransactions = []}, ())
        )
    putStrLn "Block is built."