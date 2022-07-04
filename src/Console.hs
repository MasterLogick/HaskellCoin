{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TypeApplications #-}
module Console where

import Text.Read (readMaybe)
import Control.Concurrent.MVar
import Data.Time.Clock
import Network.Socket

import CryptoMagic
import MinerState
import TBlock
import Prompts
import Commiter
import Sender
import Explorer
import NetworkMagic
import AccountBalance
import HelpCommand
import FilesMagic
import CryptoHandler

-- | Types of commands.
data Command
    = Exit_
    | Commit TransactionCandidate
    | BuildAndSend
    | Show
    | Connect String String
    | Balance SenderHash
    | LoadBlocks Path
    | WriteBlocks Path
    | StartServer String String
    | GeneratePair
    | ShowPair
    | Id
    | Help

-- | Exits from programm.
handleExit_ :: Handler
handleExit_ stateRef = do
    modifyMVar stateRef (\miner -> 
        return (miner{shouldExit = True},())) 
    startParseWrite stateRef
    putStrLn "Bye!"

-- | Handles command.
handleCommand :: Command -> Handler
handleCommand command = case command of
  Exit_ -> handleExit_
  BuildAndSend -> buildAndSendToNet
  Commit trans -> commitTransaction trans
  Show -> exploreNetwork
  Connect ip port -> connectAndSync ip port 
  Balance id_sender -> userBalance id_sender
  StartServer ip port -> setupServer ip port
  LoadBlocks filePath -> loadBlocks filePath
  WriteBlocks filePath -> writeBlocks filePath
  GeneratePair -> genPair
  ShowPair -> printPair
  Id -> getId
  Help -> printHelp


-- | Parses command.
parseCommand :: SenderHash -> UTCTime -> String -> Maybe Command
parseCommand sender time input =
    case input of
        "exit" -> Just Exit_
        "build" -> Just BuildAndSend
        "show" -> Just Show
        "generate" -> Just GeneratePair
        "key"  -> Just ShowPair
        "id" -> Just Id
        "help" -> Just Help
        _ ->
            case words input of
                ["commit", id_reciver, amt] -> 
                    case readMaybe amt of
                        Nothing -> Nothing
                        Just amount -> 
                                case readMaybe id_reciver of
                                    Nothing -> Nothing
                                    Just reciver -> Just (Commit (TransactionCandidate sender reciver amount time))
                ["connect", ip, port] ->
                    Just (Connect ip port)
                ["balance", idSender] ->
                    case readMaybe idSender of
                        Nothing -> Nothing
                        Just id_sender -> Just (Balance id_sender)
                    -- Just (Balance id_sender)
                ["start", "server", ip, port] ->
                    Just (StartServer ip port)
                ["loadFile", path] ->
                    Just (LoadBlocks path)
                ["writeFile", path] ->
                    Just (WriteBlocks path)
                _ -> Nothing

-- | Prints greeting to console.
printGreeting :: IO()
printGreeting = do
    putStrLn "+-------------------------------------------------+"
    putStrLn "| _   _           _        _ _  ____      _       |"
    putStrLn "|| | | | __ _ ___| | _____| | |/ ___|___ (_)_ __  |"
    putStrLn "|| |_| |/ _` / __| |/ / _ \\ | | |   / _ \\| | '_ \\ |"
    putStrLn "||  _  | (_| \\__ \\   <  __/ | | |__| (_) | | | | ||"
    putStrLn "||_| |_|\\__,_|___/_|\\_\\___|_|_|\\____\\___/|_|_| |_||"
    putStrLn "|                                                 |"
    putStrLn "|The best blockchain written in the best language.|"
    putStrLn "+-------------------------------------------------+"
    putStrLn "    https://github.com/MasterLogick/HaskellCoin   "
    putStrLn "                                                 "
    putStrLn "Print help to get command list and description."

    
-- | Default entry point.
run :: IO ()
run = withSocketsDo $ do
    initMinerState' <- newMVar (MinerState {
        blocks = [genesisBlock],
        pendingTransactions =  [],
        network = [],
        keyPair = fallbackPair,
        hashId = hashFunc $ snd fallbackPair,
        shouldExit = False
        })
    printGreeting
    startParseCommand initMinerState'
    startParseBackup initMinerState'
    startParseListen initMinerState'
    startParseConnect initMinerState'
    putStrLn "Welcome to Haskell coin blockchain!"
    mainLoop initMinerState' parseCommand handleCommand

-- | Processes commands.
mainLoop
    :: MVar MinerState
    -> (SenderHash -> UTCTime -> String -> Maybe Command)
    -> (Command -> Handler)
    -> IO ()
mainLoop stateRef parser handler = do
    byteInput <- prompt "command> "
    let input = validateUserInput byteInput
    time <- getCurrentTime
    minerState <- readMVar stateRef
    let sender = hashId minerState
    case parser sender time input of
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

