{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TypeApplications #-}
module Prompts where

import Control.Concurrent.MVar
import MinerState
import qualified Control.Exception as CE 
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64.URL as DBU
import qualified Data.ByteString as DBS
import Data.ByteString
import System.IO
import CryptoHandler
import CryptoMagic
import FilesMagic
import NetworkManagement

data ResponceJudgement = Accepted | Reject | WrongResponce | Default

-- | Outputs and inputs in concole with prompt.
prompt :: String -> IO ByteString
prompt text = do
    System.IO.putStr text
    hFlush stdout
    Data.ByteString.getLine

validateUserInput :: ByteString -> String
validateUserInput input =
  case C8.unpack $ DBS.take 1 $ DBS.reverse input of
    "\r" -> C8.unpack $ DBS.reverse $ DBS.drop 1 $ DBS.reverse input
    _ -> C8.unpack input

checkUserResponse :: String -> ResponceJudgement
checkUserResponse response = 
    case response of
        "Yes" -> Accepted
        "YES" -> Accepted
        "yes" -> Accepted
        "y"   -> Accepted
        "Y"   -> Accepted
        "No"  -> Reject
        "no"  -> Reject
        "n"   -> Reject
        "N"   -> Reject
        ""    -> Default
        _     -> WrongResponce

-- | Starts parsing command for getting private key.
startParseCommand :: MVar MinerState -> IO()
startParseCommand minerState = do
    putStrLn "Do you want to generate new private key? YES/no"
    byteInput <- prompt "answer> "
    let input = validateUserInput byteInput
    case checkUserResponse input of
        Accepted -> accept
        Reject -> reject
        Default -> accept
        WrongResponce -> do
            putStrLn "ERROR: unrecognized command"
            startParseCommand minerState
    where 
        accept = genPair minerState
        reject = do
            privateKey <- prompt "Enter your private key: "
            publicKey <- CE.evaluate $ generatePublic (DBU.decodeLenient $ validated privateKey)
            modifyMVar_ minerState (\miner -> return miner{keyPair = (DBU.decodeLenient $ validated privateKey, publicKey), hashId = hashFunc publicKey})
            where
              validated key = 
                case C8.unpack $ DBS.take 1 $ DBS.reverse key of
                  "\r" -> DBS.reverse $ DBS.drop 1 $ DBS.reverse key
                  _ -> key


-- | Starts parsing command for uploading backup of chain from file.
startParseBackup :: MVar MinerState -> IO()
startParseBackup stateRef = do
    putStrLn "Do you want to load backup of chain from file? yes/NO"
    byteInput <- prompt "answer> "
    let input = validateUserInput byteInput
    case checkUserResponse input of
        Accepted -> accept
        Reject -> reject
        Default -> reject
        WrongResponce -> do
            putStrLn "ERROR: unrecognized command"
            startParseBackup stateRef
    where 
        accept = do
            path <- prompt "Enter your path: "
            let userPath = validateUserInput path
            loadBlocks userPath stateRef
        reject = return ()

-- | Starts parsing command for connecting to the network.
startParseConnect :: MVar MinerState -> IO()
startParseConnect stateRef = do
    putStrLn "Do you want to connect to the network? yes/NO"
    byteInput <- prompt "answer> "
    let input = validateUserInput byteInput
    case checkUserResponse input of
        Accepted -> accept
        Reject -> reject
        Default -> reject
        WrongResponce -> do
            putStrLn "ERROR: unrecognized command"
            startParseConnect stateRef
    where 
        accept = do
            ip <- prompt "Enter your remote ip: "
            port <- prompt "Enter your remote port: "
            let userIp = validateUserInput ip
            let userPort = validateUserInput port
            connectAndSync userIp userPort stateRef
        reject = return ()

-- | Starts parsing command for listening network.
startParseListen :: MVar MinerState -> IO()
startParseListen stateRef = do
    putStrLn "Do you want to start listening? yes/NO"
    byteInput <- prompt "answer> "
    let input = validateUserInput byteInput
    case checkUserResponse input of
        Accepted -> accept
        Reject -> reject
        Default -> reject
        WrongResponce -> do
            putStrLn "ERROR: unrecognized command"
            startParseListen stateRef
    where 
        accept = do
            ip <- prompt "Enter your ip: "
            port <- prompt "Enter your port: "
            let userIp = validateUserInput ip
            let userPort = validateUserInput port
            setupServer userIp userPort stateRef
        reject = return ()

-- | Starts parsing command for writing backup of chain into file.
startParseWrite :: MVar MinerState -> IO()
startParseWrite stateRef = do
    putStrLn "Do you want to write backup of chain into file? YES/no"
    byteInput <- prompt "answer> "
    let input = validateUserInput byteInput
    case checkUserResponse input of
        Accepted -> accept
        Reject -> reject
        Default -> accept
        WrongResponce -> do
            putStrLn "ERROR: unrecognized command"
            startParseBackup stateRef
    where 
        accept = do
            path <- prompt "Enter your path: "
            let userPath = validateUserInput path
            writeBlocks userPath stateRef
        reject = return ()