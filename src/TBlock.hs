module TBlock where

data Block = Block PrevHash MinerHash Nonce TransCount TransList

data Transaction = Transaction SenderHash RecvHash Amount Signature

type Signature = Integer
type SenderHash = BlockHash
type RecvHash = BlockHash
type Amount = Double
type PrevHash = BlockHash
type MinerHash = BlockHash
type BlockHash = Integer
type Nonce = Integer
type TransCount = Int
type TransList = [Transaction]