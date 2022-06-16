{-# LANGUAGE FlexibleInstances #-}
module TBlock where

import Data.Binary
import qualified Data.ByteString as DBY hiding (unpack)
import Data.ByteArray hiding (reverse)
import Crypto.Hash
import qualified Data.ByteArray as Data
data Block = Block PrevHash MinerHash Nonce TransCount TransList

hashFunc :: DBY.ByteString -> Digest SHA1
hashFunc = hash

-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
                 
instance Binary Block where
    put (Block prevHash minerHash nonce transCount transList) = do
        put prevHash
        put minerHash
        put nonce
        put transCount
        putList transList

    get = do
        prevHash <- get :: Get PrevHash
        minerHash <- get :: Get MinerHash
        nonce <- get :: Get Nonce
        transCount <- get :: Get TransCount
        transList <- getMany transCount :: Get TransList
        return (Block prevHash minerHash nonce transCount transList)

data Transaction = Transaction SenderHash RecvHash Amount Signature

instance Binary Transaction where
    put (Transaction sender receiver amount sig) = do
        put sender
        put receiver
        put amount
        put sig

    get = do
        sender <- get :: Get SenderHash
        receiver <- get :: Get RecvHash
        amount <- get :: Get Amount
        sig <- get :: Get Signature
        return (Transaction sender receiver amount sig)

instance Binary BlockHash where
    put digest = do
        put $ DBY.pack $ unpack digest

    get = do
        digest <- get :: Get BlockHash
        return $ extract $ digestFromByteString digest
        where
            extract :: Maybe BlockHash -> BlockHash
            extract Nothing = hashFunc $ DBY.toStrict $ encode (0 :: Int) -- hash of (0 :: Int)
            extract (Just digest) = digest

type Signature = Integer
type SenderHash = BlockHash
type RecvHash = BlockHash
type Amount = Double
type PrevHash = BlockHash
type MinerHash = BlockHash
type BlockHash = Digest SHA1
type Nonce = Integer
type TransCount = Int
type TransList = [Transaction]