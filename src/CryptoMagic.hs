{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CryptoMagic where

import Data.Proxy
import Data.Binary
import Data.ByteString
import Data.ByteArray
import Data.ByteArray.Encoding
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Base64.URL as DBU
import qualified Data.ByteArray as Data
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as B
import Crypto.ECC
    ( Curve_P521R1,
      EllipticCurve(curveGenerateKeyPair),
      KeyPair(keypairGetPublic, keypairGetPrivate) )
import Crypto.Hash
import Crypto.Error
import Crypto.Random.Types
import Crypto.PubKey.ECDSA as ECDSA
import Control.Monad
import Basement.Compat.Base
import  Data.Binary.Get

-- | Saving of block's hash.
type BlockHash = Digest SHA1

-- | Enum For Getting Key From Pair Tuple
data GetKey = Private | Public deriving Eq
type Pair = (ByteString, ByteString)

type HSignature = (ByteString, ECDSA.Signature Curve_P521R1)

-- | Hash function.
hashFunc :: DBS.ByteString -> Digest SHA1
hashFunc = hash


-- | For the curve we are using, the parameters are
-- | PubKey length - 133 bytes
-- | PrivKey length - 66 bytes

-- | Fallback hash to use in case of some extraordinary situations
fallbackHash :: BlockHash
fallbackHash = hashFunc $ DBS.toStrict $ encode (0 :: Int)

-- | This is a special hash that starts with 00 byte. In real blockchain systems this parameter is different, 
-- so it makes the problem of searching for nonce much harder. For the sake of testing & simplicity we decided
-- to make this particular problem easy
ruleHash :: BlockHash
ruleHash = hashFunc $ DBS.toStrict $ encode (783 ::Int)

-- | Fallback keypair to use in case of some extraordinary situations
fallbackPair :: Pair
fallbackPair = (DBU.decodeLenient $ C8.pack "BADoaSa2fzVfzsDUjbwiADEGZPVvy0SB6RUSm95qeY5fClNXHzUyhUojTvhJOfhY3_JBt2ujIjg0WBt-DEPOB2pIwgCoTpW2qcCqS2pd6P5PvgAFHiWL_5KFEOt3pNZTmUspvK3Sl1FQ1866xodvn2bfLacr-9xGnCONzBzEi5i9Ajqg1w==",
               DBU.decodeLenient $ C8.pack "AUcQUJ2Ag6I2c-12b9WJcHzJNuqFWMC8nOOdEt-M_fzJOL-8wCpp0pviir_9Jnq3LCd3y4rp86YGfDdaVyIxfLv2")

-- | Fallback signature to use in case of some extraordinary situations
fallbackSignature :: IO (ECDSA.Signature Curve_P521R1)
fallbackSignature = signMsg priv msg
    where
        priv = getKeyFromPair Private fallbackPair
        msg = C8.pack "\000"

-- | Curve proxy
proxy :: Proxy Curve_P521R1
proxy = Proxy :: Proxy Curve_P521R1

-- | Create (PrivKey, PubKey) pair using random 
createEncodedKeys :: IO Pair
createEncodedKeys = do
    keypair <- curveGenerateKeyPair proxy
    let pubKey = keypairGetPublic keypair
    let privKey = keypairGetPrivate keypair
    let privE = encodePrivate proxy privKey :: ByteString
    let pubE = encodePublic proxy pubKey :: ByteString
    return (privE, pubE)

-- | Extract Key from Pair 
getKeyFromPair :: GetKey -> Pair -> ByteString
getKeyFromPair gk (priv, pub)
    | gk == Private = priv
    | otherwise = pub

-- | Generate PublicKey using PrivateKey
generatePublic :: ByteString -> ByteString
generatePublic priv = encodePublic proxy (toPublic proxy dPriv)
    where
        dPriv = throwCryptoError (decodePrivate proxy priv)

-- | Sign message using PrivateKey.
signMsg :: (MonadRandom m) => ByteString -> ByteString -> m (ECDSA.Signature Curve_P521R1)
signMsg priv msg = sign proxy dPriv SHA256 eMsg
    where
        dPriv = throwCryptoError (decodePrivate proxy priv)
        eMsg  = msg

-- | Verify if string message was signed with matching PrivateKey (Function gets PublicKey)
verifyStringMsg :: ByteString -> ECDSA.Signature Curve_P521R1 -> Bool
verifyStringMsg pub sig = verify proxy SHA1 dPub sig eMsg
    where
        dPub = throwCryptoError (decodePublic proxy pub)
        eMsg = C8.pack ""

-- | Make the signature hashable
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

-- | Make block hashable.
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