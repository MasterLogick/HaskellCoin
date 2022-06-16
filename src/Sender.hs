module Sender where

import Crypto.Hash
import qualified Data.ByteString as DBY
import Data.Binary
import Data.ByteArray
import MinerState
import TBlock 

getLast :: [a] -> Maybe a
getLast [] = Nothing
getLast [e] = Just e
getLast (e:es) = getLast es

blockHash :: Maybe Block -> BlockHash
blockHash Nothing = hashFunc $ DBY.toStrict $ Data.Binary.encode (0 :: Int) -- hash of (0 :: Int)
blockHash (Just block)
     = hashFunc (DBY.toStrict $ encode block)

buildAndSendToNet :: Handler
buildAndSendToNet state = do
    putStrLn "Block is built."
    return $ Just state{blocks = newChain, pendingTransactions = []} 
        where
           blockchain = blocks state
           pending = pendingTransactions state
           newChain :: [Block]
           newChain = newBlock : blockchain

           hashedPrev :: BlockHash
           hashedPrev = blockHash (getLast blockchain)

           newBlock :: Block
           newBlock = Block hashedPrev (blockHash Nothing) 0 (Prelude.length pending) pending