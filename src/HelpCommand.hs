module HelpCommand where

import MinerState

printHelp :: Handler
printHelp _ = do
    putStrLn "exit                                              - exit from program"
    putStrLn "build                                             - build new block"
    putStrLn "show                                              - print chain of blocks"
    putStrLn "commit [senderId] [recieverId] [amountMoney]  - create transaction"
    putStrLn "connect [ip] [port]                               - connect to server at specified ip and port"
    putStrLn "balance [userHash]                                - print user's balance"
    putStrLn "id                                                - print user's id"
    putStrLn "generate                                          - generate & print new key pair"
    putStrLn "key                                               - print current user's key pair"
    putStrLn "writeFile /path/to/file                           - generate & write the whole blockchain to the file"
    putStrLn "loadFile /path/to/file                            - load the whole blockchain from the file"
    putStrLn "start server [ip] [port]                          - start server at specified ip and port"