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
import NetworkRules


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


Request pending transactions:
1) Client Sends:
    "Gimme pend trans" :: String

2) Server sends:
    transList :: [Transaction]

3) Profit!!!


Send new transaction:
1) Server sends:
    "New pend trans" :: String
    transaction :: Transcation

2) Profit!!!

Send new block:
1) Server sends:
    "New block" :: String
    block :: Block

2) Profit!!!

-}

{-
Some comments about internal work of this module:
Library for sockets in this implementation of network module has only blocking instance of recv function, so one of the main goals of this module is to asynchronously handle everything and not to block everything.
For this purpose every active socket connection has it's own thread that constantly reads data from the socket and pushes it to the buffer of incoming data. After every read it tries to parse data and handle incoming request if the user is free for servicing.
To provide mutual access for user socket user object has nuService flag. This flag is set iff some thread interacts with user and all over threads must wait. Otherwise, user is free to be serviced. Use withService if you want to have a mutual access to the user.
-}

-- | Type for network incoming request handlers.
type NetRequestHandler = MVar MinerState -> NetUser -> IO Bool

-- | Network infrastructure management functions.

-- | Connsole command "start server" handler.
-- | Starts server socket and forks with listening loop.
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

-- | Server socket listening loop.
-- | Records incoming connections and starts client request handlers.
serverHandleLoop :: MVar MinerState -> Socket -> IO ()
serverHandleLoop stateRef serverSocket = do
    (client, addr) <- accept serverSocket
    putStrLn $ "New connection from " ++ (show addr)
    handleNewConnection stateRef client
    serverHandleLoop stateRef serverSocket

-- | Console command "connect" handler.
-- | Connects to the specified server and forks with client request handler.
connectAndSync :: String -> String -> Handler
connectAndSync ip port stateRef = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    putStrLn $ "Connection to " ++ (show $ addrAddress $ addr) ++ " established"
    user <- handleNewConnection stateRef sock
    peerName <- getPeerName sock
    mHash <- requestLastBlockHash stateRef user
    case mHash of
        Nothing -> gracefulClose sock 3000
        Just blockHash -> do
            putStrLn $ "Recieved last block hash " ++ (show blockHash) ++ " from " ++ (show peerName)

-- | Records new connection in miner state and forks with client request handler.
handleNewConnection :: MVar MinerState -> Socket -> IO NetUser
handleNewConnection stateRef sock = do
    user <- newNetUser sock
    modifyMVar_ stateRef (\miner -> return miner{ network = (user:(network miner)) })
    forkIO $ (clientInputHandleLoop stateRef user)
    return user

-- | Saves all incoming data in user's buffer and tries to handle requests if user is not in the service mode.
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



-- | Incoming request handlers.

-- | Global handler for all requests.
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
                "New pend trans" -> handleNewPendingTranscation stateRef user
                "Gimme last hash" -> handleLastHashRequest stateRef user
                "Gimme block" -> handleBlockRequest stateRef user
                "Gimme pend trans" -> handlePendingTransactionsRequest stateRef user
                _ -> return False

-- | Handles "New pend trans" message and validates it.
handleNewPendingTranscation :: NetRequestHandler
handleNewPendingTranscation stateRef user = do
    trans' <- receive user :: IO (Maybe Transaction)
    case trans' of
        Nothing -> return False
        Just trans -> modifyMVar stateRef (\minerState -> do
            let chain = blocks minerState
            let pendingTrans = pendingTransactions minerState
            peerName <- getPeerName (nuSocket user)
            if elem trans pendingTrans then
                return (minerState, True)
            else if validateWholeChain chain (trans:pendingTrans) then do
                putStrLn ("Accepted new pending transaction from " ++ (show peerName))
                propagateLastPendingTransactionToNet stateRef
                return (minerState{ pendingTransactions = pendingTrans ++ [trans] }, True)
            else do
                putStrLn ("Declined new pending transaction from " ++ (show peerName))
                return (minerState, False)
                )

-- | Handles "New block" message and validates it.
-- todo remove built transactions from pending list
handleNewBlock :: NetRequestHandler
handleNewBlock stateRef user = do
    block' <- receive user :: IO (Maybe Block)
    case block' of
        Nothing -> return False
        Just block -> do
            modifyMVar stateRef (\minerState -> do
                let acceptance = judgeBlock minerState block
                peerName <- getPeerName (nuSocket user)
                case acceptance of
                    Accept -> do
                        putStrLn ("Accepted new block " ++ (show (getBlockHash block)) ++ " from " ++ (show peerName))
                        propagateLastBlockToNet stateRef
                        return (minerState{ blocks = (block:(blocks minerState)) }, True)
                    AlreadyPresent -> do
                        return (minerState, True)
                    BranchDivergence -> do
                        divergedPart' <- receiveDivergedPart block user minerState
                        case divergedPart' of
                            Nothing -> return (minerState, False)
                            Just divergence' -> do
                                let divergence = block:divergence'
                                state <- case mergeBranches divergence (blocks minerState) of
                                    Left merged -> do
                                        putStrLn ("Local branch selected")
                                        return minerState{ blocks = merged }   
                                    Right merged -> do
                                        putStrLn ("Remote branch selected")
                                        return minerState{ blocks = merged }
                                propagateLastBlockToNet stateRef
                                return (state, True)
                )

-- | Helper function for handleNewBlock.
-- | Fetches the diverged part of the blockchain from the user.
receiveDivergedPart :: Block -> NetUser -> MinerState -> IO(Maybe [Block])
receiveDivergedPart newestDivergedBlock user minerState = do
    let prevHash = bPrevHash newestDivergedBlock
    sendAll (nuSocket user) (encode "Gimme block")
    mPrevBlock <- receive user :: (IO (Maybe Block))
    case mPrevBlock of
        Nothing -> return Nothing
        Just receivedBlock -> do
            let acceptance = judgeBlock minerState receivedBlock
            case acceptance of
                Accept -> do
                    return (Just [receivedBlock, (getNewestBlock minerState)])
                AlreadyPresent -> do
                    return (Just [receivedBlock])
                BranchDivergence -> do
                    mDivergedRemainder <- receiveDivergedPart receivedBlock user minerState
                    return (fmap (receivedBlock:) mDivergedRemainder)

-- | Handles "Gimme last hash" request.
handleLastHashRequest :: NetRequestHandler
handleLastHashRequest stateRef user = do
    minerState <- readMVar stateRef
    let lastBlockHash = getBlockHash (getNewestBlock minerState)
    sendAll (nuSocket user) (encode lastBlockHash)
    return True

-- | Handles "Gimme block" request.
handleBlockRequest :: NetRequestHandler
handleBlockRequest stateRef user = do
    requestedHash' <- receive user :: IO (Maybe BlockHash)
    case requestedHash' of
        Nothing -> return False
        Just requestedHash -> do
            minerState <- readMVar stateRef
            let requestedBlock' = getBlockByHash minerState requestedHash 
            case requestedBlock' of
                Nothing -> sendAll (nuSocket user) (encode "Fail")
                Just requestedBlock -> do
                    sendAll (nuSocket user) (encode "Ok")
                    sendAll (nuSocket user) (encode requestedBlock)
            return True

-- | Handles "Gimme pend trans" request.
handlePendingTransactionsRequest :: NetRequestHandler
handlePendingTransactionsRequest stateRef user = do
    minerState <- readMVar stateRef
    sendAll (nuSocket user) (encode (pendingTransactions minerState))
    return True



-- | Functions for active interaction with network.

-- | Propagates the newest block in the miner to the network.
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

-- | Sends the specified block to the user.
sendBlock :: MVar MinerState -> Block -> NetUser -> IO ()
sendBlock stateRef block user = withService stateRef user $ do
    let sock = nuSocket user
    sockName <- getPeerName sock
    sendAll sock (encode "New block")
    sendAll sock (encode block)
    putStrLn $ "Sent block to " ++ (show sockName)

-- | Propagates the newest pending transaction in the miner to the network.
propagateLastPendingTransactionToNet :: Handler
propagateLastPendingTransactionToNet stateRef = do
    forkIO (do
        minerState <- readMVar stateRef
        let net = network minerState
        let mTrans = listToMaybe (reverse (pendingTransactions minerState))
        case mTrans of
            Nothing -> return ()
            Just trans -> do
                foldMap (sendTrans stateRef trans) net
                putStrLn $ "Sent pending transaction to " ++ (show $ length $ net) ++ " users"
        )
    return ()

-- | Sends the specified block to the user.
sendTrans :: MVar MinerState -> Transaction -> NetUser -> IO ()
sendTrans stateRef trans user = withService stateRef user $ do
    let sock = nuSocket user
    sockName <- getPeerName sock
    sendAll sock (encode "New pend trans")
    sendAll sock (encode trans)
    putStrLn $ "Sent pending transaction to " ++ (show sockName)

-- | Fetches the last hash form the user's blockchain.
requestLastBlockHash :: MVar MinerState -> NetUser -> IO (Maybe BlockHash)
requestLastBlockHash stateRef user = withService stateRef user $ do
    let sock = nuSocket user
    sockName <- getPeerName sock
    sendAll sock (encode "Gimme last hash")
    receive user :: IO (Maybe BlockHash)

-- | Internal state management functions.
-- | Waits until the user is free for servicing and services it.
withService :: MVar MinerState -> NetUser -> IO a -> IO a
withService stateRef user i = do
    beginService user
    ret <- i
    endService stateRef user
    return ret

-- | Waits until the user is free for servicing and starts servicing.
beginService :: NetUser -> IO ()
beginService user = do
    canService' <- canService user
    unless canService' (beginService user)

-- | Stops servicing.
endService :: MVar MinerState -> NetUser -> IO ()
endService stateRef user = do
        handleIncommingMessage stateRef user
        swapMVar (nuService user) False
        return ()

-- | Checks if the user is free for servicing.
canService :: NetUser -> IO Bool
canService user = modifyMVar (nuService user) (\isBusy -> return (True, not isBusy))



-- | Helper function.
-- | Returns the data object from the incoming buffer of the user.
-- todo add timeout
receive :: Binary a => NetUser -> IO (Maybe a)
receive user = join $ modifyMVar (nuBuffer user) (\buffer ->
        case decodeOrFail buffer of
            Left _ -> return (buffer, receive user)
            Right (remainder, _, parsedVal) -> return (remainder, return (Just parsedVal))
        )
