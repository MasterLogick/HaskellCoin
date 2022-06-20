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
import CryptoMagic


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

type NetRequestHandler = MVar MinerState -> NetUser -> IO Bool

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
    chunk <- recv (nuSocket user) 4096
    shouldStop <- modifyMVar (nuBuffer user) (\buffer -> do
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
                let newBuffer = LB.append buffer chunk
                return (newBuffer, False)
        )
    unless shouldStop $ do
        canService' <- canService user
        when canService' $ do
            forkIO $ do
                success <- handleIncommingMessage stateRef user
                swapMVar (nuService user) False
                unless success $ gracefulClose (nuSocket user) 3000
            return ()
        clientInputHandleLoop stateRef user

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
    putStrLn $ "Connection to " ++ (show $ addrAddress $ addr) ++ " established"
    handleNewConnection stateRef sock



handleIncommingMessage :: NetRequestHandler
handleIncommingMessage stateRef user = do
    let bufferRef = nuBuffer user
    buffer <- takeMVar bufferRef
    let request :: Either (LB.ByteString, ByteOffset, String) (LB.ByteString, ByteOffset, String); request = decodeOrFail buffer
    case request of
        Left (_, _, err) -> do
            putMVar bufferRef buffer
            return True
        Right (remainder, consumedSize, tag) -> do
            putMVar bufferRef remainder
            case tag of
                "New block" -> handleNewBlock stateRef user
                "Gimme last hash" -> handleLastHashRequest stateRef user
                _ -> return False

handleNewBlock :: NetRequestHandler
handleNewBlock stateRef user = do
    block' <- recieve user :: IO (Maybe Block)
    case block' of
        Nothing -> return False
        Just block -> do
            peerName <- getPeerName (nuSocket user)
            putStrLn $ "Got new block from " ++ (show peerName)
            modifyMVar_ stateRef (\miner -> return miner{ blocks = (block:(blocks miner)) })
            return True

handleLastHashRequest :: NetRequestHandler
handleLastHashRequest stateRef user = do
    minerState <- readMVar stateRef
    let lastBlockHash = hashFunc $ LB.toStrict $ encode $ getNewestBlock $ minerState
    sendAll (nuSocket user) (encode lastBlockHash)
    return True



propagateLastBlockToNet :: Handler
propagateLastBlockToNet stateRef = do
    forkIO (modifyMVar_ stateRef (\miner -> do
        let net = network miner
        let newestBlock = getNewestBlock miner
        foldMap (sendBlock stateRef newestBlock) net
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
    canService' <- canService user
    unless canService' (beginService user)

endService :: MVar MinerState -> NetUser -> IO ()
endService stateRef user = do
        handleIncommingMessage stateRef user
        swapMVar (nuService user) False
        return ()

canService :: NetUser -> IO Bool
canService user = modifyMVar (nuService user) (\isBusy -> return (True, not isBusy))



-- todo add timeout
recieve :: Binary a => NetUser -> IO (Maybe a)
recieve user = join $ modifyMVar (nuBuffer user) (\buffer ->
        case decodeOrFail buffer of
            Left _ -> return (buffer, recieve user)
            Right (remainder, _, parsedVal) -> return (remainder, return (Just parsedVal))
        )
