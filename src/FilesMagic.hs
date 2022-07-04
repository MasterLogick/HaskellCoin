{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module FilesMagic where 

import Data.Binary
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy as LB
    
import MinerState
import NetworkRules
import NetworkMagic


type Path = String

-- | Loads blocks from file using Path.
loadBlocks :: Path -> Handler
loadBlocks filePath stateRef = do
    bytes <- LB.readFile filePath
    case decodeOrFail bytes of
        Left _ -> putStrLn "Something wrong!"
        Right (_, _, blocksData) -> modifyMVar_ stateRef (\minerState -> do
            let present = blocks minerState
            case selectChain blocksData present of
                Left merged -> do
                    if validateWholeChain merged [] then do
                        putStrLn "Got new blocks from file"
                        propagateLastBlockToNet stateRef
                        return minerState{ blocks = merged, pendingTransactions = [] }
                    else do
                        putStrLn "Chain backup is mailformed"
                        return minerState
                Right merged -> do
                    putStrLn "Local chain is better"
                    return minerState{ blocks = merged }
            )

-- | Writes blocks to file using Path.
writeBlocks :: Path -> Handler
writeBlocks filePath stateRef = do
    miner <- readMVar stateRef
    LB.writeFile filePath (encode (blocks miner))
    return ()
