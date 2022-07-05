{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Explorer where

import Control.Concurrent.MVar

import MinerState

-- | Prints all blocks in console.
exploreNetwork :: Handler
exploreNetwork stateRef = do
    minerState <- readMVar stateRef
    putStrLn $ foldMap (\x -> (show x) ++ "\n .   .   .   .   .   .   .   .   . \n/|\\ /|\\ /|\\ /|\\ /|\\ /|\\ /|\\ /|\\ /|\\\n |   |   |   |   |   |   |   |   | \n") (blocks minerState)


printPending :: Handler
printPending stateRef = do
    minerState <- readMVar stateRef
    putStrLn $ foldMap (\x -> (show x) ++ "\n") (pendingTransactions minerState)