{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CryptoMagic where

import Data.Proxy
import Data.Binary
import Data.ByteString
import Data.ByteArray
import Data.ByteArray.Encoding
import qualified Data.ByteString as DBS
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
import Data.Binary.Get

-- | Saving of block's hash.
type BlockHash = Digest SHA1

-- | Enum For Getting Key From Pair Tuple
data GetKey = Private | Public deriving Eq
type Pair = (ByteString, ByteString)

type HSignature = (ByteString, ECDSA.Signature Curve_P521R1)
--type HSignature = Integer

-- | Hash function.
hashFunc :: DBS.ByteString -> Digest SHA1
hashFunc = hash


-- | For the curve we are using, the parameters are
-- | PubKey length - 133 bytes
-- | PrivKey length - 66 bytes

-- | Fallback hash to use in case of some extraordinary situations
fallbackHash :: BlockHash
fallbackHash = hashFunc $ DBS.toStrict $ encode (0 :: Int)

fallbackPair :: Pair
fallbackPair = (C8.pack $ Prelude.concat $ Prelude.replicate 133 "\000",
               C8.pack $ Prelude.concat $ Prelude.replicate 66 "\000")

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

printSignatureInteger :: IO (Signature Curve_P521R1)
printSignatureInteger = do
    sig <- join func
    return sig

func :: IO (IO (ECDSA.Signature Curve_P521R1))
func = do
    pair <- createEncodedKeys
    let pub = getKeyFromPair Public pair
    let pri = getKeyFromPair Private pair
    let msg = toStrict $ encode (123 :: Int)

    let signat = signMsg pri msg :: IO (ECDSA.Signature Curve_P521R1)
    return signat

test = do
    signature <- printSignatureInteger
    let right = toStrict $ encode signature
    let wrong = fromStrict (DBS.drop 10 right)

    let decoded = decodeOrFail $ fromStrict right :: Either (B.ByteString, Data.Binary.Get.ByteOffset, String) (B.ByteString, ByteOffset, ECDSA.Signature Curve_P521R1)

    return decoded

test1 = do
    ei <- test
    return $ f ei
    where
        f (Right (_, _, sig)) = sig

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