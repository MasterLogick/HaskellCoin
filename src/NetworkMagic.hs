module NetworkMagic where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Data.Binary
import Data.Binary.Get
import Data.Maybe
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Data.ByteString.Lazy as LB

import MinerState
import TBlock


{-

HaskellCoin network protocol:


Here client and server are just conventional names for requesting and responding sides. Actually both sides act as server and client.


Request hash of the last block in the chain:
1) Client sends:
    "Gimme last hash" :: String

2) Server sends:
    "Ok" :: String      -- if server has blocks in the chain (lol, almost always)
    hash :: BlockHash

    or 

    "Fail" :: String -- if server doesn't have any blocks in the chain

3) Profit!!!


Request block by hash:
1) Client sends:
    "Gimme block" :: String
    hash :: BlockHash

2) Server sends:
    "Ok" :: String      -- if server has requested block in the chain
    block :: Block

    or 

    "Fail" :: String -- if server doesn't have requested block in the chain

3) Profit!!!


Send new block:
1) Server sends:
    "New block" :: String
    block :: Block

2) Profit!!!

-}

setupServer :: String -> String -> Handler
setupServer ip port stateRef = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
    serverSocket <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind serverSocket (addrAddress addr)
    listen serverSocket 1024
    putStrLn $ "Start listening for connections on " ++ (show $ addrAddress $ addr)
    setSocketOption serverSocket ReuseAddr 1
    forkIO $ (serverHandleLoop stateRef serverSocket)
    return ()

serverHandleLoop :: MVar MinerState -> Socket -> IO ()
serverHandleLoop stateRef serverSocket = do
    (client, addr) <- accept serverSocket
    putStrLn $ "New connection from " ++ (show addr)
    handleNewConnection stateRef client
    serverHandleLoop stateRef serverSocket

clientInputHandleLoop :: MVar MinerState -> NetUser -> IO ()
clientInputHandleLoop stateRef user = do
    shouldStop <- modifyMVar (nuState user) (\state -> do
            chunk <- recv (nuSocket user) 1024
            if (LB.null chunk) then do
                peerName <- getPeerName $ nuSocket $ user
                putStrLn $ "Connection to " ++ (show $ peerName) ++ " lost"
                modifyMVar stateRef (\minerState -> 
                    return (minerState{ network = 
                        filter (\x -> (nuSocket user) /= (nuSocket x)) (network minerState)
                        }, ())
                    )
                return (state, True)
            else do
                let newBuff = LB.append (nuBuffer state) chunk
                let newState = state{ nuBuffer = newBuff }
                if (nuService state) then
                    return (newState, False)
                else do
                    updatedState <- tryHandle stateRef user newState
                    return (updatedState, False)
                )
    unless shouldStop (clientInputHandleLoop stateRef user)

handleNewConnection :: MVar MinerState -> Socket -> IO ()
handleNewConnection stateRef sock = do
    user <- newNetUser sock
    modifyMVar stateRef (\miner -> return (miner{ network = (user:(network miner)) },()))
    forkIO $ (clientInputHandleLoop stateRef user)
    return ()

connectAndSync :: String -> String -> Handler
connectAndSync ip port stateRef = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    handleNewConnection stateRef sock



tryHandle :: MVar MinerState -> NetUser -> NetUserState -> IO NetUserState
tryHandle stateRef user newState = do
    let request :: Either (LB.ByteString, ByteOffset, String) (LB.ByteString, ByteOffset, String); request = decodeOrFail (nuBuffer newState)
    case request of
        Left (_, _, err) -> do
            return newState
        Right (remainder, _, "New block") -> do
            let block :: Either (LB.ByteString, ByteOffset, String) (LB.ByteString, ByteOffset, Block); block = decodeOrFail remainder
            case block of
                Left (_, _, nerr) -> do
                    return newState
                Right (remainder', _, block') -> do
                    peerName <- getPeerName $ nuSocket $ user
                    putStrLn $ "Got new block from " ++ (show $ peerName)
                    modifyMVar stateRef (\miner -> return (miner{ blocks = (block':(blocks miner)) }, newState{ nuBuffer = remainder' }))



propagateLastBlockToNet :: Handler
propagateLastBlockToNet stateRef = do
    forkIO (modifyMVar stateRef (\miner -> do
        let net = network miner
        case listToMaybe $ blocks $ miner of
            Nothing -> return (miner, ())
            Just newBlock -> do
                foldMap (sendBlock stateRef newBlock) net
                return (miner, ())
        putStrLn $ "Sent block to " ++ (show $ length $ net) ++ " users" 
        return (miner, ())
        ))
    return ()



sendBlock :: MVar MinerState -> Block -> NetUser -> IO ()
sendBlock stateRef block user = do
    let sock = nuSocket user
    sockName <- getPeerName sock
    sendAll sock $ encode "New block"
    sendAll sock $ encode block
    putStrLn $ "Sent block to " ++ (show sockName)



withService :: MVar MinerState -> NetUser -> IO a -> IO a
withService stateRef user i = do
    beginService user
    ret <- i
    endService stateRef user
    return ret

beginService :: NetUser -> IO ()
beginService user = do
    canStart' <- modifyMVar (nuState user) (\state -> do
        let canStart = ((LB.null (nuBuffer state)) && (nuService state))
        if canStart then
            return (state{ nuService = True }, canStart)
        else
            return (state, canStart)
        )
    unless canStart' (beginService user)

endService :: MVar MinerState -> NetUser -> IO ()
endService stateRef user = modifyMVar (nuState user) (\state -> do
    let modifiedState = state { nuService = False }
    newState <- tryHandle stateRef user modifiedState
    return (newState, ())
    )
