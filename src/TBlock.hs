module TBlock where

import Data.Time.Clock
import Data.Binary
import qualified Data.ByteString.Lazy as LB

import CryptoMagic

-- | Data Block stores previous block hash, miner hash, nonce of block, 
-- | amount of transactions and list of all transactions that corespond to this block.
data Block = Block {
    bPrevHash :: PrevHash,
    bMinerHash :: MinerHash,
    bNonce :: Nonce,
    bTransCount :: TransCount,
    bTransList :: TransList
}

-- | First block of chain.
genesisBlock :: Block
genesisBlock = Block {
    bPrevHash = fallbackHash,
    bMinerHash = hashFunc $ getKeyFromPair Public fallbackPair,
    bNonce = 0,
    bTransCount = 0,
    bTransList = []
}

-- | Fallback block to use in case of some extraordinary situations.
fallbackBlock :: Block
fallbackBlock = Block fallbackHash fallbackHash 0 0 []

-- | Converts Block to BlockHash using hash function.
getBlockHash :: Block -> BlockHash
getBlockHash b = hashFunc (LB.toStrict (encode b))

-- | This instance is neccessary for converting block into bytes and also bytes into data block.               
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

-- | Data Transaction stores information about sender, recipient, amount of transaction
-- | and also signature.
data Transaction = Transaction {
    tSender :: SenderHash,
    tReceiver :: RecvHash,
    tAmount :: Amount,
    tTime :: UTCTime,
    tSignature :: HSignature
} deriving Eq

-- | This instance is neccessary for converting transactions into bytes and also bytes into data block.
instance Binary TransactionCandidate where
    put transactionCandidate = do
        put (tcSender transactionCandidate)
        put (tcReceiver transactionCandidate)
        put (tcAmount transactionCandidate)
        put (tcTime transactionCandidate)


    get = do
        sender <- get :: Get SenderHash
        receiver <- get :: Get RecvHash
        amount <- get :: Get Amount
        time <- get :: Get UTCTime
        return (TransactionCandidate{ tcSender = sender, tcReceiver = receiver, tcAmount = amount, tcTime = time })

instance Binary Transaction where
    put transcation = do
        put (tSender transcation)
        put (tReceiver transcation)
        put (tAmount transcation)
        put (tTime transcation)
        put (tSignature transcation)
    
    get = do
        sender <- get :: Get SenderHash
        receiver <- get :: Get RecvHash
        amount <- get :: Get Amount
        time <- get :: Get UTCTime 
        signature <- get :: Get HSignature
        return (Transaction{ tSender = sender, tReceiver = receiver, tAmount = amount, tTime = time, tSignature = signature })

instance Binary UTCTime where
    put time = do
        put (toRational (utctDayTime time))
        put (fromEnum (utctDay time))

    get = do
        dayTime <- get :: Get Rational
        day <- get :: Get Int
        return (UTCTime{ utctDay = toEnum day, utctDayTime = fromRational dayTime })


-- | Neccessary type aliases.
data TransactionCandidate = TransactionCandidate {
    tcSender :: SenderHash,
    tcReceiver :: RecvHash,
    tcAmount :: Amount,
    tcTime :: UTCTime
}

type SenderHash = BlockHash
type RecvHash = BlockHash
type Amount = Double
type PrevHash = BlockHash
type MinerHash = BlockHash
type Nonce = Integer
type TransCount = Int
type TransList = [Transaction]
