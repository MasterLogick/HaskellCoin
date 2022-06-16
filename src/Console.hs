module Console where

import MinerState
import TBlock
import Commiter
import Sender
import Explorer

--Data.Text.Lazy.IO
import Text.Read (readMaybe)

import System.IO

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

makeProgram :: a -> IO a
makeProgram = return

type Handler = MinerState -> (IO (), Maybe MinerState)


handleExit_ :: Handler
handleExit_ _ = (putStrLn "Bye!", Nothing)

handleBuild :: Handler
handleBuild ms = (putStrLn "Block is built.", Just (buildAndSendToNet ms))

handleCommit :: Transaction -> Handler
handleCommit t ms = (putStrLn  "Transaction is added to pending block.", Just (commitTransaction t ms))

handleShow :: Handler
handleShow = exploreNetwork

handleCommand :: Command -> Handler
handleCommand command = case command of
  Exit_ -> handleExit_
  BuildAndSend -> handleBuild
  Commit trans -> handleCommit trans
  Show -> handleShow
        
-- | Parse a task manager bot command.
--
-- >>> parseCommand "/done 3"
-- Just (RemoveTask 3)
-- >>> parseCommand "/done 1 2 3"
-- Nothing
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
                _ -> Nothing

-- | Init state of the system
initMinerState :: MinerState
initMinerState = MinerState [] []

-- | Default entry point.
run :: IO ()
run = runWith initMinerState parseCommand handleCommand

runWith
    :: state
    -> (String -> Maybe command)
    -> (command -> state -> (IO (), Maybe state))
    -> IO () 
runWith tasks parse handle = do
    input <- prompt "command> "
    case parse input of
        Nothing -> do
            putStrLn "ERROR: unrecognized command"
            runWith tasks parse handle
        Just command' -> do
            case handle command' tasks of
                (feedback, newTasks) -> do
                    feedback
                    case newTasks of
                        Nothing -> return ()
                        Just newTasks' -> do
                            runWith newTasks' parse handle
