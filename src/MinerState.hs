{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module MinerState where

import Control.Concurrent.MVar
import Network.Socket
import Data.Maybe
import qualified Data.ByteString.Lazy as LB

import TBlock
import CryptoMagic

-- | Data MinerState storers information about blocks, pendings transactions,
-- | network and boole flag on exit.
data MinerState = MinerState {
    blocks :: [Block],
    pendingTransactions :: [Transaction],
    network :: [NetUser],
    keyPair :: Pair,
    hashId :: MinerHash,
    shouldExit :: Bool
}

-- | The type of commands available to enter into the console.
type Handler = MVar MinerState -> IO ()

-- | Input buffer.
type InBuffer = LB.ByteString

-- | User type.
data NetUser = NetUser {
    nuSocket :: Socket,
    nuBuffer :: MVar InBuffer,
    nuService :: MVar Bool
}

-- | Creates new user.
newNetUser :: Socket -> IO NetUser
newNetUser sock = do
    buffer <- newMVar LB.empty
    service <- newMVar False
    return (NetUser sock buffer service)

-- | Gets last block.
getNewestBlock :: MinerState -> Block
getNewestBlock minerState = fromMaybe fallbackBlock (listToMaybe (blocks minerState))

-- | Gets block by hash.
getBlockByHash :: MinerState -> BlockHash -> Maybe Block
getBlockByHash minerState hash = listToMaybe (filter filterRule (blocks minerState))
    where
        filterRule block = (getBlockHash block) == hash