module Console where

import Text.Read (readMaybe)
import Control.Concurrent.MVar
import System.IO
import Network.Socket

import MinerState
import TBlock
import Commiter
import Sender
import Explorer
import NetworkMagic
import Balance

-- | Output and input in concole with prompt.
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- | Types of commands
data Command
    = Exit_
    | Commit Transaction
    | BuildAndSend
    | Show
    | Connect String String
<<<<<<< HEAD
    | Balance SenderHash
=======
    | StartServer String String
>>>>>>> 777f30b3f6e9adc46d7d717753a2b6b742380264

-- | Exit from programm
handleExit_ :: Handler
handleExit_ stateRef = do
    modifyMVar stateRef (\miner -> 
        return (miner{shouldExit = True},())) 
    putStrLn "Bye!"

-- | Handling of command.
handleCommand :: Command -> Handler
handleCommand command = case command of
  Exit_ -> handleExit_
  BuildAndSend -> buildAndSendToNet
  Commit trans -> commitTransaction trans
  Show -> exploreNetwork
  Connect ip port -> connectAndSync ip port 
<<<<<<< HEAD
  Balance id_sender -> userBalance id_sender
=======
  StartServer ip port -> setupServer ip port
>>>>>>> 777f30b3f6e9adc46d7d717753a2b6b742380264

-- | Parsing of command.
parseCommand :: String -> Maybe Command
parseCommand input =
    case input of
        "exit" -> Just Exit_
        "build" -> Just BuildAndSend
        "show" -> Just Show
        _ ->
            case words input of
                ["commit", id_sender, id_reciver, amt] -> 
                    case readMaybe amt of
                        Nothing -> Nothing
                        Just amount -> 
                            case readMaybe id_sender of
                                Nothing -> Nothing
                                Just sender ->
                                    case readMaybe id_reciver of
                                        Nothing -> Nothing
                                        Just reciver -> Just (Commit (Transaction sender reciver amount 0))
                ["connect", ip, port] ->
                    Just (Connect ip port)
                ["balance", idSender] ->
                    case readMaybe idSender of
                        Nothing -> Nothing
                        Just id_sender -> Just (Balance id_sender)
                    -- Just (Balance id_sender)
                ["start", "server", ip, port] ->
                    Just (StartServer ip port)
                _ -> Nothing

-- | Default entry point.
run :: IO ()
run = withSocketsDo $ do
    initMinerState' <- newMVar (MinerState [] [] [] False)
    mainLoop initMinerState' parseCommand handleCommand

-- | Processing of commands.
mainLoop
    :: MVar MinerState
    -> (String -> Maybe Command)
    -> (Command -> Handler)
    -> IO ()
mainLoop stateRef parser handler = do
    input <- prompt "command> "
    case parser input of
        Nothing -> do
            putStrLn "ERROR: unrecognized command"
            mainLoop stateRef parser handler
        Just command -> do
            handler command stateRef
            miner <- readMVar stateRef
            if shouldExit miner then 
                return ()
            else
                mainLoop stateRef parser handler
