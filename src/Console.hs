module Console where

--Data.Text.Lazy.IO
import Text.Read (readMaybe)
import Control.Concurrent.MVar
import System.IO

import MinerState
import TBlock
import Commiter
import Sender
import Explorer
import NetworkMagic
import Balance


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

data Command
    = Exit_
    | Commit Transaction
    | BuildAndSend
    | Show
    | Connect String String
    | Balance SenderHash

handleExit_ :: Handler
handleExit_ stateRef = do
    modifyMVar stateRef (\miner -> 
        return (miner{shouldExit = True},())) 
    putStrLn "Bye!"

handleCommand :: Command -> Handler
handleCommand command = case command of
  Exit_ -> handleExit_
  BuildAndSend -> buildAndSendToNet
  Commit trans -> commitTransaction trans
  Show -> exploreNetwork
  Connect ip port -> connectAndSync ip port 
  Balance id_sender -> userBalance id_sender

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
                _ -> Nothing

-- | Default entry point.
run :: IO ()
run = do
    initMinerState' <- newMVar (MinerState [] [] [] False)
    mainLoop initMinerState' parseCommand handleCommand

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
