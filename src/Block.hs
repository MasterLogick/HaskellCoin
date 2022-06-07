module Block where

data Block = Block PrevHash MinerHash Nonce TransCount TransList

data Transaction = Transaction [(SenderHash, RecvHash, Amount)]

type SenderHash = BlockHash
type RecvHash = BlockHash
type Amount = Double
type PrevHash = BlockHash
type MinerHash = BlockHash
type BlockHash = Integer
type Nonce = Integer
type TransCount = Integer
type TransList = [Transaction]