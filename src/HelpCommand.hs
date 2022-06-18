module HelpCommand where

import MinerState

printHelp :: Handler
printHelp _ = do
    putStrLn "exit - exit from programm"
    putStrLn "build - build new block and send it to network"
    putStrLn "show - print chain of blocks"
    putStrLn "commit [senderHash] [recieverHash] [amountMoney] - create transaction"
    putStrLn "connect [ip] [port] - connect to server at specified ip and port"
    putStrLn "balance [userHash] - print user's balance"
    putStrLn "start server [ip] [port] - start server at specified ip and port"