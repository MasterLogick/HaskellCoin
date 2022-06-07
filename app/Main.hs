module Main where

import Block

data NetState = NetState [Block] [Trans]

run :: IO ()
run = putStrLn "someFunc"

main :: IO ()
main = run
