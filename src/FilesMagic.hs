module FilesMagic where 
    
import Control.Concurrent.MVar

import MinerState
import TBlock

import Data.Binary
import qualified Data.ByteString.Lazy as LB
-- import System.Directory

import Control.Concurrent.MVar

type Path = String

loadBlocks :: Path -> Handler
loadBlocks filePath stateRef = do
    bytes <- LB.readFile filePath
    case decodeOrFail bytes of 
        Left _ -> putStrLn "Something wrong!"
        Right (_, _, blocksData) -> modifyMVar_ stateRef (\minerState -> return minerState{ blocks = blocksData })
    return ()

writeBlocks :: Path -> Handler
writeBlocks filePath stateRef = do
    miner <- readMVar stateRef
    LB.writeFile filePath (encode (blocks miner))
    return ()