module Main where

import Block

data NetState = NetState [Block] [Trans]

f x = x + 1

run :: IO ()
run = putStrLn "someFunc"

main :: IO ()
main = run
