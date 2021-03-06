{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module CryptoMagic where

import Data.Proxy
import Data.Binary
import Data.ByteString
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Base64.URL as DBU
import qualified Data.ByteArray as Data
import qualified Data.ByteString.Char8 as C8
import Crypto.ECC
    ( Curve_P521R1,
      EllipticCurve(curveGenerateKeyPair),
      KeyPair(keypairGetPublic, keypairGetPrivate),
      Point(..) )
import Crypto.Hash
import Crypto.Error
import Crypto.Random.Types
import Crypto.PubKey.ECDSA as ECDSA

-- | Save of block's hash.
type BlockHash = Digest SHA1

-- | Enum for getting key from pair tuple.
data GetKey = Private | Public deriving Eq
type Pair = (ByteString, ByteString)

type HSignature = (ByteString, ECDSA.Signature Curve_P521R1)

-- | Hash function.
hashFunc :: DBS.ByteString -> Digest SHA1
hashFunc = hash


-- | For the curve we are using the parameters which are
-- | PubKey length - 133 bytes
-- | PrivKey length - 66 bytes

-- | Fallback hash to use in case of some extraordinary situations.
fallbackHash :: BlockHash
fallbackHash = hashFunc $ DBS.toStrict $ encode (0 :: Int)

-- | Fallback keypair to use in case of some extraordinary situations.
fallbackPair :: Pair
fallbackPair = (DBU.decodeLenient $ C8.pack "BADoaSa2fzVfzsDUjbwiADEGZPVvy0SB6RUSm95qeY5fClNXHzUyhUojTvhJOfhY3_JBt2ujIjg0WBt-DEPOB2pIwgCoTpW2qcCqS2pd6P5PvgAFHiWL_5KFEOt3pNZTmUspvK3Sl1FQ1866xodvn2bfLacr-9xGnCONzBzEi5i9Ajqg1w==",
               DBU.decodeLenient $ C8.pack "AUcQUJ2Ag6I2c-12b9WJcHzJNuqFWMC8nOOdEt-M_fzJOL-8wCpp0pviir_9Jnq3LCd3y4rp86YGfDdaVyIxfLv2")

-- | Fallback signature to use in case of some extraordinary situations.
fallbackSignature :: IO (ECDSA.Signature Curve_P521R1)
fallbackSignature = signMsg priv msg
    where
        priv = getKeyFromPair Private fallbackPair
        msg = C8.pack "\000"

-- | Curve proxy.
proxy :: Proxy Curve_P521R1
proxy = Proxy :: Proxy Curve_P521R1

-- | Creates (PrivKey, PubKey) pair using random.
createEncodedKeys :: IO Pair
createEncodedKeys = do
    keypair <- curveGenerateKeyPair proxy
    let pubKey = keypairGetPublic keypair
    let privKey = keypairGetPrivate keypair
    let privE = encodePrivate proxy privKey :: ByteString
    let pubE = encodePublic proxy pubKey :: ByteString
    return (privE, pubE)

-- | Extracts Key from Pair.
getKeyFromPair :: GetKey -> Pair -> ByteString
getKeyFromPair gk (priv, pub)
    | gk == Private = priv
    | otherwise = pub

-- | Generates PublicKey using PrivateKey.
generatePublic :: ByteString -> ByteString
generatePublic priv = encodePublic proxy (toPublic proxy dPriv)
    where
        dPriv = throwCryptoError (decodePrivate proxy priv)

-- | Signs message using PrivateKey.
signMsg :: (MonadRandom m) => ByteString -> ByteString -> m (ECDSA.Signature Curve_P521R1)
signMsg priv msg = sign proxy dPriv SHA256 eMsg
    where
        dPriv = throwCryptoError (decodePrivate proxy priv)
        eMsg  = msg

-- | Verifies if string message was signed with matching PrivateKey (Function gets PublicKey).
verifyStringMsg :: ByteString -> ByteString -> ECDSA.Signature Curve_P521R1 -> Bool
verifyStringMsg pub msg sig = unfail failablePoint
    where
        failablePoint = decodePoint proxy pub
        eMsg = msg

        unfail :: CryptoFailable (Point Curve_P521R1) -> Bool
        unfail (CryptoPassed point) = verify proxy SHA256 point sig eMsg
        unfail (CryptoFailed _error) = False

-- | Makes the signature hashable.
instance Binary (ECDSA.Signature Curve_P521R1) where
    put signature = do
        put $ signatureToIntegers proxy signature

    get = do
        sigPair <- get :: Get (Integer, Integer)
        let failableSignature = signatureFromIntegers proxy sigPair
        unfail failableSignature
        where
            unfail :: CryptoFailable (ECDSA.Signature Curve_P521R1) -> Get (ECDSA.Signature Curve_P521R1)
            unfail (CryptoPassed signature) = return signature
            unfail (CryptoFailed error_) = fail $ show error_

-- | Makes block hashable.
instance Binary BlockHash where
    put digest = do
        put $ DBS.pack $ Data.unpack digest
    
    get = do
        digest <- get :: Get DBS.ByteString
        return $ extract $ digestFromByteString digest
        where
            extract :: Maybe BlockHash -> BlockHash
            extract Nothing = hashFunc $ DBS.toStrict $ encode (0 :: Int) -- hash of (0 :: Int)
            extract (Just digest) = digest 