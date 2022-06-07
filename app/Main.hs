{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where
--import      Block
import TBlock --(Transaction, Block)

data MinerState = MinerState [Block] [Transaction]

f x = x + 1

run :: IO ()
run = putStrLn "someFunc"

main :: IO ()
main = run
