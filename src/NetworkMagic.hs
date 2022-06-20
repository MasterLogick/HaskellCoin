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
Client always sends String that identifies request in first of all. 


Request hash of the last block in the chain:
1) Client sends:
    "Gimme last hash" :: String

2) Server sends:
    hash :: BlockHash

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

type NetRequestHandler = MVar MinerState -> NetUser -> InBuffer -> IO (Maybe InBuffer)

setupServer :: String -> String -> Handler
setupServer ip port stateRef = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
    serverSocket <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption serverSocket ReuseAddr 1
    bind serverSocket (addrAddress addr)
    listen serverSocket 1024
    putStrLn $ "Start listening for connections on " ++ (show $ addrAddress $ addr)
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
    shouldStop <- modifyMVar (nuBuffer user) (\buffer -> do
            chunk <- recv (nuSocket user) 1024
            if (LB.null chunk) then do
                peerName <- getPeerName $ nuSocket $ user
                putStrLn $ "Connection to " ++ (show $ peerName) ++ " lost"
                modifyMVar_ stateRef (\minerState -> 
                    return minerState{ network = 
                        filter (\x -> (nuSocket user) /= (nuSocket x)) (network minerState)
                        }
                    )
                return (buffer, True)
            else do
                let newBuff = LB.append buffer chunk
                needService <- modifyMVar (nuService user)
                    (\isServiced -> return (True, not isServiced))
                if needService then do
                    mayRem <- tryHandle stateRef user newBuff
                    swapMVar (nuService user) False
                    return (fromMaybe newBuff mayRem, False)
                else do
                    return (newBuff, False)
                )
    unless shouldStop (clientInputHandleLoop stateRef user)

handleNewConnection :: MVar MinerState -> Socket -> IO ()
handleNewConnection stateRef sock = do
    user <- newNetUser sock
    modifyMVar_ stateRef (\miner -> return miner{ network = (user:(network miner)) })
    forkIO $ (clientInputHandleLoop stateRef user)
    return ()

connectAndSync :: String -> String -> Handler
connectAndSync ip port stateRef = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    handleNewConnection stateRef sock



tryHandle :: NetRequestHandler
tryHandle stateRef user newBuffer = do
    let request :: Either (LB.ByteString, ByteOffset, String) (LB.ByteString, ByteOffset, String); request = decodeOrFail newBuffer
    case request of
        Left (_, _, err) -> do
            putStrLn $ "Handle error: " ++ err
            return Nothing
        Right (remainder, _, "New block") -> do
            handleNewBlock stateRef user remainder
        Right _ -> do
            return Nothing

handleNewBlock :: NetRequestHandler
handleNewBlock stateRef user newBuffer = do
    let block :: Either (LB.ByteString, ByteOffset, String) (LB.ByteString, ByteOffset, Block); block = decodeOrFail newBuffer
    case block of
        Left _ -> do
            return Nothing
        Right (remainder, _, block') -> do
            peerName <- getPeerName $ nuSocket $ user
            putStrLn $ "Got new block from " ++ (show peerName)
            modifyMVar stateRef (\miner -> return (miner{ blocks = (block':(blocks miner)) }, Just remainder))



propagateLastBlockToNet :: Handler
propagateLastBlockToNet stateRef = do
    forkIO (modifyMVar_ stateRef (\miner -> do
        let net = network miner
        -- todo make code cleaner
        case listToMaybe $ blocks $ miner of
            Nothing -> return ()
            Just newBlock -> do
                foldMap (sendBlock stateRef newBlock) net
                return ()
        putStrLn $ "Sent block to " ++ (show $ length $ net) ++ " users"
        return miner
        ))
    return ()



sendBlock :: MVar MinerState -> Block -> NetUser -> IO ()
sendBlock stateRef block user = withService stateRef user $ do
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
    canStart <- modifyMVar (nuService user) 
        (\isServiced -> return (True, not isServiced))
    unless canStart (beginService user)

endService :: MVar MinerState -> NetUser -> IO ()
endService stateRef user = do
    swapMVar (nuService user) False
    return ()
