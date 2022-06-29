module TBlock where

import Data.Binary
import qualified Data.ByteString.Lazy as LB

import CryptoMagic

-- | data Block stores previous block hash, miner hash, nonce of block, 
-- | amount of transactions and list of all transactions that corespond to this block
data Block = Block {
    bPrevHash :: PrevHash,
    bMinerHash :: MinerHash,
    bNonce :: Nonce,
    bTransCount :: TransCount,
    bTransList :: TransList
}

genesisBlock :: Block
genesisBlock = Block {
    bPrevHash = fallbackHash,
    bMinerHash = hashFunc $ getKeyFromPair Public fallbackPair,
    bNonce = 0,
    bTransCount = 0,
    bTransList = []
}

-- | Fallback block to use in case of some extraordinary situations
fallbackBlock :: Block
fallbackBlock = Block fallbackHash fallbackHash 0 0 []

getBlockHash :: Block -> BlockHash
getBlockHash b = hashFunc (LB.toStrict (encode b))

-- | this instance is neccessary for converting block into bytes and also bytes into data block                 
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
        transList <- get :: Get TransList
        return (Block prevHash minerHash nonce transCount transList)

instance Eq Block where
    (==) block1 block2 = getBlockHash block1 == getBlockHash block2

-- | data Transaction stores information about sender, recipient, amount of transaction
-- | and also signature
data Transaction = Transaction SenderHash RecvHash Amount HSignature

-- | this instance is neccessary for converting transactions into bytes and also bytes into data block 
instance Binary TransactionCandidate where
    put (TransactionCandidate sender receiver amount) = do
        put sender
        put receiver
        put amount

    get = do
        sender <- get :: Get SenderHash
        receiver <- get :: Get RecvHash
        amount <- get :: Get Amount
        return (TransactionCandidate sender receiver amount)

instance Binary Transaction where
    put (Transaction sender receiver amount signature) = do
        put sender
        put receiver
        put amount
        put signature
    
    get = do
        sender <- get :: Get SenderHash
        receiver <- get :: Get RecvHash
        amount <- get :: Get Amount 
        signature <- get :: Get HSignature
        return (Transaction sender receiver amount signature)

data TransactionCandidate = TransactionCandidate SenderHash RecvHash Amount
-- | neccessary type aliases
type SenderHash = BlockHash
type RecvHash = BlockHash
type Amount = Double
type PrevHash = BlockHash
type MinerHash = BlockHash
type Nonce = Integer
type TransCount = Int
type TransList = [Transaction]