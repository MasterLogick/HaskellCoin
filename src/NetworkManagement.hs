{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module NetworkManagement where

import Control.Concurrent
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Maybe
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Data.ByteString.Lazy as LB
import Data.Time.Clock
import Control.Exception hiding (Handler)

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
    putStrLn $ "Start listening for connections on " ++ show (addrAddress addr)
    _ <- forkIO (serverHandleLoop stateRef serverSocket)
    return ()

-- | Server socket listening loop.
-- | Records incoming connections and starts client request handlers.
serverHandleLoop :: MVar MinerState -> Socket -> IO ()
serverHandleLoop stateRef serverSocket = do
    (client, addr) <- accept serverSocket
    putStrLn $ "New connection from " ++ show addr
    _ <- handleNewConnection stateRef client
    serverHandleLoop stateRef serverSocket

-- | Console command "connect" handler.
-- | Connects to the specified server and forks with client request handler.
connectAndSync :: String -> String -> Handler
connectAndSync ip port stateRef = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    putStrLn $ "Connection to " ++ show (addrAddress addr) ++ " established"
    user <- handleNewConnection stateRef sock
    shouldnotClose <- withService stateRef user $ modifyMVar stateRef $ \minerState -> do
        mHash <- requestLastBlockHash user
        case mHash of
            Nothing -> do
                gracefulClose sock 3000
                return (minerState, False)
            Just blockHash -> if (blockHash /= getBlockHash (getNewestBlock minerState)) then do
                    mBlock <- requestBlock user blockHash
                    case mBlock of
                        Nothing -> return (minerState, False)
                        Just block -> do
                            let acceptance = preliminaryJudgeBlock minerState block
                            case acceptance of
                                AlreadyPresent -> do
                                    sendBlock (getNewestBlock minerState) user
                                    return (minerState, True)
                                Accept -> acceptBlock block user minerState stateRef
                                BranchDivergence -> resolveBranchDivergence user stateRef minerState
                else
                    return (minerState, True)
    unless shouldnotClose (gracefulClose sock 3000)

-- | Records new connection in miner state and forks with client request handler.
handleNewConnection :: MVar MinerState -> Socket -> IO NetUser
handleNewConnection stateRef sock = do
    user <- newNetUser sock
    modifyMVar_ stateRef (\miner -> return miner{ network = user:(network miner) })
    _ <- forkIO (clientInputHandleLoop stateRef user)
    return user

-- | Saves all incoming data in user's buffer and tries to handle requests if user is not in the service mode.
clientInputHandleLoop :: MVar MinerState -> NetUser -> IO ()
clientInputHandleLoop stateRef user = do
    eChunc <- try $ recv (nuSocket user) 4096 :: IO (Either SomeException LB.ByteString)
    shouldStop <- case eChunc of
        Left _ -> return True
        Right chunk -> modifyMVar (nuBuffer user) $ \buffer -> do
            if LB.null chunk then do
                modifyMVar_ stateRef $ \minerState ->
                    return minerState{ network =
                        filter (\x -> nuSocket user /= nuSocket x) (network minerState)
                        }
                return (buffer, True)
            else do
                let newBuffer = LB.append buffer chunk
                return (newBuffer, False)
    if shouldStop then do
        peerName <- safeGetPeerName (nuSocket user)
        putStrLn ("Connection to " ++ peerName ++ " lost")
    else do
        canService' <- canService user
        when canService' $ do
            _ <- forkIO $ do
                success <- handleIncommingMessage stateRef user
                _ <- swapMVar (nuService user) False
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
        Left (_, _, _) -> do
            putMVar bufferRef buffer
            return True
        Right (remainder, _, tag) -> do
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
        Just trans -> modifyMVar stateRef $ \minerState -> do
            let chain = blocks minerState
            let pendingTrans = pendingTransactions minerState
            if trans `elem` pendingTrans then
                return (minerState, True)
            else if validateWholeChain chain (pendingTrans ++ [trans]) then do
                peerName <- safeGetPeerName (nuSocket user)
                putStrLn ("Accepted new pending transaction from " ++ peerName)
                propagateLastPendingTransactionToNet stateRef
                return (minerState{ pendingTransactions = pendingTrans ++ [trans] }, True)
            else do
                peerName <- safeGetPeerName (nuSocket user)
                putStrLn ("Declined new pending transaction from " ++ peerName)
                return (minerState, False)

-- | Handles "New block" message and validates it.
handleNewBlock :: NetRequestHandler
handleNewBlock stateRef user = do
    block' <- receive user :: IO (Maybe Block)
    case block' of
        Nothing -> return False
        Just block ->
            modifyMVar stateRef $ \minerState -> do
                let acceptance = preliminaryJudgeBlock minerState block
                case acceptance of
                    Accept -> do
                        acceptBlock block user minerState stateRef
                    AlreadyPresent -> do
                        return (minerState, True)
                    BranchDivergence -> do
                        peerName <- safeGetPeerName (nuSocket user)
                        putStrLn ("Branch divergence " ++ peerName ++ " with user detected")
                        resolveBranchDivergence user stateRef minerState

-- | Accept new block
acceptBlock :: Block -> NetUser -> MinerState -> MVar MinerState -> IO (MinerState, Bool)
acceptBlock block user minerState stateRef = do
    let newTranses = filter (\x -> notElem x (bTransList block)) (pendingTransactions minerState)
    if validateWholeChain (block:(blocks minerState)) newTranses then do
        peerName <- safeGetPeerName (nuSocket user)
        putStrLn ("Accepted new block " ++ (show (getBlockHash block)) ++ " from " ++ peerName)
        propagateLastBlockToNet stateRef
        return (minerState{ blocks = (block:(blocks minerState)), pendingTransactions = newTranses }, True)
    else do
        return (minerState, False)

-- | Resolves branch divergence conflicts
resolveBranchDivergence :: NetUser -> MVar MinerState -> MinerState -> IO (MinerState, Bool)
resolveBranchDivergence user stateRef minerState = do
    sendAll (nuSocket user) (encode ("Gimme last hash" :: String))
    mHash <- receive user :: IO (Maybe BlockHash)
    case mHash of
        Nothing -> return (minerState, False)
        Just hash -> do
            mBlock <- requestBlock user hash
            case mBlock of
                Nothing -> return (minerState, False)
                Just block -> do 
                    mDivergedPart <- receiveDivergedPart block user minerState
                    case mDivergedPart of
                        Nothing -> return (minerState, False)
                        Just divergence' -> do
                            let divergence = block:divergence'
                            retVal <- case selectChain divergence (blocks minerState) of
                                Left merged -> do
                                    transList' <- requestTransList user
                                    case transList' of
                                        Nothing -> return (minerState, False)
                                        Just transList -> do
                                            if validateWholeChain merged transList then do
                                                putStrLn "Remote branch selected"
                                                return (minerState{ blocks = merged, pendingTransactions = transList }, True)
                                            else do
                                                putStrLn "Local branch selected by validation"
                                                return (minerState, False)
                                Right _ -> do
                                    putStrLn "Local branch selected by rules"
                                    return (minerState, True)
                            propagateLastBlockToNet stateRef
                            return retVal

-- | Helper function for handleNewBlock.
-- | Fetches the diverged part of the blockchain from the user.
receiveDivergedPart :: Block -> NetUser -> MinerState -> IO(Maybe [Block])
receiveDivergedPart newestDivergedBlock user minerState = do
    let prevHash = bPrevHash newestDivergedBlock
    mPrevBlock <- requestBlock user prevHash
    case mPrevBlock of
        Nothing -> return Nothing
        Just receivedBlock -> do
            let acceptance = preliminaryJudgeBlock minerState receivedBlock
            case acceptance of
                Accept ->
                    return (Just [receivedBlock, (getNewestBlock minerState)])
                AlreadyPresent ->
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
                Nothing -> sendAll (nuSocket user) (encode ("Fail" :: String))
                Just requestedBlock -> do
                    sendAll (nuSocket user) (encode ("Ok" :: String))
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
    _ <- forkIO $ do
        minerState <- readMVar stateRef
        let net = network minerState
        let newestBlock = getNewestBlock minerState
        foldMap (\user -> withService stateRef user (sendBlock newestBlock user)) net
        putStrLn $ "Sent block to " ++ show (length net) ++ " users"
    return ()

-- | Sends the specified block to the user.
sendBlock :: Block -> NetUser -> IO ()
sendBlock block user = do
    let sock = nuSocket user
    sendAll sock (encode ("New block" :: String))
    sendAll sock (encode block)
    peerName <- safeGetPeerName (nuSocket user)
    putStrLn ("Sent block to " ++ peerName)

-- | Fetches the specified block from the user
requestBlock :: NetUser -> BlockHash -> IO (Maybe Block)
requestBlock user hash = do
    sendAll (nuSocket user) (encode ("Gimme block" :: String))
    sendAll (nuSocket user) (encode hash)
    code <- receive user :: IO (Maybe String)
    case code of
        Just "Ok" -> receive user :: (IO (Maybe Block))
        _ -> return Nothing

-- | Propagates the newest pending transaction in the miner to the network.
propagateLastPendingTransactionToNet :: Handler
propagateLastPendingTransactionToNet stateRef = do
    _ <- forkIO $ do
        minerState <- readMVar stateRef
        let net = network minerState
        let mTrans = listToMaybe (reverse (pendingTransactions minerState))
        case mTrans of
            Nothing -> return ()
            Just trans -> do
                foldMap (sendTrans stateRef trans) net
                putStrLn $ "Sent pending transaction to " ++ show (length $ net) ++ " users"
    return ()

-- | Sends the specified block to the user.
sendTrans :: MVar MinerState -> Transaction -> NetUser -> IO ()
sendTrans stateRef trans user = withService stateRef user $ do
    let sock = nuSocket user
    sendAll sock (encode ("New pend trans" :: String))
    sendAll sock (encode trans)
    peerName <- safeGetPeerName (nuSocket user)
    putStrLn ("Sent pending transaction to " ++ peerName)

-- | Fetches the last hash form the user's blockchain.
requestLastBlockHash :: NetUser -> IO (Maybe BlockHash)
requestLastBlockHash user = do
    sendAll (nuSocket user) (encode ("Gimme last hash" :: String))
    receive user :: IO (Maybe BlockHash)

-- | Fetches the list of pending transactions form the user
requestTransList :: NetUser -> IO (Maybe [Transaction])
requestTransList user = do
    sendAll (nuSocket user) (encode ("Gimme pend trans" :: String))
    receive user :: IO (Maybe [Transaction])

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
        _ <- handleIncommingMessage stateRef user
        _ <- swapMVar (nuService user) False
        return ()

-- | Checks if the user is free for servicing.
canService :: NetUser -> IO Bool
canService user = modifyMVar (nuService user) (\isBusy -> return (True, not isBusy))



-- | Helper function.
-- | Returns the data object from the incoming buffer of the user.
-- todo add timeout
receive :: Binary a => NetUser -> IO (Maybe a)
receive user = do
    time <- getCurrentTime
    let timeEnd = addUTCTime (3 :: NominalDiffTime) time
    receive' timeEnd
    where
        receive' et = do
            time <- getCurrentTime
            if fromEnum (diffUTCTime et time) < 0 then
                return Nothing
            else
                join $ modifyMVar (nuBuffer user) $ \buffer ->
                    case decodeOrFail buffer of
                        Left _ -> return (buffer, receive' et)
                        Right (remainder, _, parsedVal) -> return (remainder, return (Just parsedVal))

-- | Gets the peer name of the socket
safeGetPeerName :: Socket -> IO String
safeGetPeerName sock = do
    mPeerName <- try (getPeerName sock) :: IO (Either SomeException SockAddr)
    case mPeerName of
        Left _ -> return "unknown"
        Right pname -> return (show pname)
