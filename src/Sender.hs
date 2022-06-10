module Sender where

import Crypto.Hash
import Data.ByteString

import MinerState
import TBlock 

sha1 :: ByteString -> Digest SHA1
sha1 = hash

getLast :: [a] -> Maybe a
getLast [] = Nothing
getLast [e] = Just e
getLast (e:es) = getLast es

blockHash :: Maybe Block -> BlockHash
blockHash Nothing = 0
blockHash (Just block) -- @(Block prevHash _minerHash _nonce _transCount _transList))
    = sha1 (encode block)
    -- = prevHash + 1

buildAndSendToNet :: MinerState -> MinerState
buildAndSendToNet (MinerState blockchain pending)
    = MinerState newChain [] 
    where
        newChain :: [Block]
        newChain = newBlock : blockchain

        hashedPrev :: BlockHash
        hashedPrev = blockHash (getLast blockchain)

        newBlock :: Block
        newBlock = Block hashedPrev 0 0 (Prelude.length pending) pending