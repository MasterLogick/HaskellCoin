module CryptoHandler where

import Control.Concurrent.MVar
import qualified Data.ByteString.Base64.URL as DBU
import CryptoMagic
import MinerState

genPair :: Handler
genPair stateRef = do
    (private, public) <- createEncodedKeys
    putStrLn "Hey buddy, here are your brand new keys, don't forget the private one:"
    putStrLn "This is your private key (psss buddy, do not share it with anyone, they will sign transactions by your name)"
    print $ DBU.encode private
    putStrLn "\nThis is your public key, feel free to share this one"
    print $ DBU.encode public
    modifyMVar stateRef (\minerState -> return (minerState{keyPair = (private, public)}, ()))

printPair :: Handler
printPair stateRef = do
    modifyMVar stateRef (\minerState -> do
        let (private, public) = keyPair minerState
        putStrLn "This is your private key (keep it secret!)"
        print $ DBU.encode private
        putStrLn "\nThis is your public key (feel free to share it)"
        print $ DBU.encode public
        return (minerState, ())
        )

getId :: Handler
getId stateRef = do
    modifyMVar stateRef (\minerState -> do
        let public = getKeyFromPair Public $ keyPair minerState
        putStrLn "This is your Id, share this one with your friend, so they will know how to send you tokens"
        print $ hashFunc public
        return (minerState, ())
        )