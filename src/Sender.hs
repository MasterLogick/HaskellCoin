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

hashFunction :: Maybe Block -> BlockHash
hashFunction Nothing = 0
hashFunction (Just (Block prevHash _minerHash _nonce _transCount _transList))
    = prevHash + 1

buildAndSendToNet :: MinerState -> MinerState
buildAndSendToNet (MinerState blockchain pending)
    = MinerState newChain [] 
    where
        newChain :: [Block]
        newChain = newBlock : blockchain

        hashedPrev :: BlockHash
        hashedPrev = hashFunction (getLast blockchain)

        newBlock :: Block
        newBlock = Block hashedPrev 0 0 (Prelude.length pending) pending