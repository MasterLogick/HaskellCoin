module Block where

data Block = Block PrevHash MinerHash Nonce TransCount TransList
data Transaction = [(SenderHash, RecvHash, Amount)]

type SenderHash = BlockHash
type RecvHash = BlockHash
type Amount = Double
type PrevHash = BlockHash
type MinerHash = BlockHash
type Nonce = Integer
type TransCount = Integer
type TransList = [Transaction]