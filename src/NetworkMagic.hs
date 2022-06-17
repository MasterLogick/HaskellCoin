module NetworkMagic where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Binary
import Data.Maybe
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)

import MinerState
import TBlock

connectAndSync :: String -> String -> Handler
connectAndSync ip port stateRef = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    modifyMVar stateRef (\miner -> return (miner{network = (sock:(network miner))},()))

propagateBlockToNet :: Handler
propagateBlockToNet stateRef = do
    forkIO (modifyMVar stateRef (\miner -> do
        let net = network miner
        case listToMaybe $ blocks $ miner of
            Nothing -> return (miner, ())
            Just newBlock -> do
                foldMap (sendBlock newBlock) net
                return (miner, ())
        putStrLn $ "Sent block to " ++ (show $ length $ net) ++ " users" 
        return (miner, ())
        ))
    return ()

sendBlock :: Block -> Socket -> IO ()
sendBlock block sock = do
    sendAll sock $ encode "New block"
    sendAll sock $ encode block
    sockName <- getPeerName sock
    putStrLn $ "Sent block to " ++ (show sockName)
