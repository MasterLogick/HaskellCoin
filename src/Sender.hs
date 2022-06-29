module Sender where

import Crypto.Hash
import qualified Data.ByteString as DBY
import Control.Concurrent.MVar
import Data.Binary
import Data.ByteArray hiding (length)
import Data.Maybe

import MinerState
import TBlock
import CryptoMagic
import NetworkMagic
import NetworkRules
import Data.Memory.Encoding.Base16

-- | function buildAndSendToNet builds and sends a block into the network
-- | and as a result, the line that the block is built
buildAndSendToNet :: Handler
buildAndSendToNet stateRef = do
    modifyMVar stateRef (\minerState -> do
        let blockchain = blocks minerState
        let pending = pendingTransactions minerState
        let hashedPrev :: BlockHash; hashedPrev = getBlockHash (fromMaybe fallbackBlock (listToMaybe blockchain))
        let newBlock :: Block; newBlock = mineBlock (Block hashedPrev (hashId minerState) 0 (length pending) pending)
        propagateLastBlockToNet stateRef
        let newChain :: [Block]; newChain = newBlock : blockchain
        return (minerState{blocks = newChain, pendingTransactions = []}, ())
        )
    putStrLn "Block is built."