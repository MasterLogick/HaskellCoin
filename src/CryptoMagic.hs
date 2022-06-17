{-# LANGUAGE FlexibleInstances #-}
module CryptoMagic where

import Data.Binary
import qualified Data.ByteString as DBY
import Crypto.Hash
import qualified Data.ByteArray as Data

type BlockHash = Digest SHA1

hashFunc :: DBY.ByteString -> Digest SHA1
hashFunc = hash

instance Binary BlockHash where
    put digest = do
        put $ DBY.pack $ Data.unpack digest

    get = do
        digest <- get :: Get DBY.ByteString
        return $ extract $ digestFromByteString digest
        where
            extract :: Maybe BlockHash -> BlockHash
            extract Nothing = hashFunc $ DBY.toStrict $ encode (0 :: Int) -- hash of (0 :: Int)
            extract (Just digest) = digest