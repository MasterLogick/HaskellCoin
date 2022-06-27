{-# LANGUAGE FlexibleInstances #-}
module CryptoMagic where

import Data.Proxy
import Data.Binary
import Data.ByteString as DBS
import Data.ByteArray
import Data.ByteArray.Encoding
import qualified Data.ByteString as DBY
import qualified Data.ByteArray as Data
import qualified Data.ByteString.Char8 as C8
import Crypto.ECC
import Crypto.Hash
import Crypto.Error
import Crypto.Random.Types
import Crypto.PubKey.ECDSA as ECDSA
import Data.Maybe (fromMaybe)

-- | Enum For Getting Key From Pair Tuple
data GetKey = Private | Public deriving Eq
type Pair = (ByteString, ByteString)

-- | Type of hash and curve
type BlockHash = Digest SHA1
type HSignature = (ByteString, ECDSA.Signature Curve_P521R1)
--type Signature = Integer

-- | For the curve we are using, the parameters are
-- | PubKey length - 133 bytes
-- | PrivKey length - 66 bytes

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

-- | Sign string message using PrivateKey. We do not actually care about signing message
signStringMsg :: (MonadRandom m) => ByteString -> ByteString -> m (ECDSA.Signature Curve_P521R1)
signStringMsg priv msg = sign proxy dPriv SHA256 eMsg
    where
        dPriv = throwCryptoError (decodePrivate proxy priv)
        eMsg  = msg

-- | Verify if string message was signed with matching PrivateKey (Function gets PublicKey)
verifyStringMsg :: ByteString -> ECDSA.Signature Curve_P521R1 -> Bool
verifyStringMsg pub sig = verify proxy SHA256 dPub sig eMsg
    where
        dPub = throwCryptoError (decodePublic proxy pub)
        eMsg = C8.pack ""

--printSignatureInteger :: (Signature Curve_P521R1) -> IO ()
printSignatureInteger = do
    tmp <- func
    sig <- tmp
    let i = signatureToIntegers proxy sig
    let r = fst i
    let s = snd i
    return $ Prelude.length $ DBS.unpack $ toStrict $ encode r

-- | Hash function
hashFunc :: DBY.ByteString -> Digest SHA1
hashFunc = hash

-- | Fallback hash to use in case of some extraordinary situations
fallbackHash :: BlockHash
fallbackHash = hashFunc $ DBY.toStrict $ encode (0 :: Int)

func :: IO (IO (ECDSA.Signature Curve_P521R1))
func = do
    pair <- createEncodedKeys
    let pub = getKeyFromPair Public pair
    let pri = getKeyFromPair Private pair
    let msg = toStrict $ encode (123 :: Int)
    
    let signat = signStringMsg pri msg :: IO (ECDSA.Signature Curve_P521R1)
    return signat

-- | Make block hashable.
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
            
instance Binary (ECDSA.Signature Curve_P521R1) where
    put signature = do
        put $ signatureToIntegers proxy signature

    get = do
        sigPair <- get :: Get (Integer, Integer)
        let failableSignature = signatureFromIntegers proxy sigPair
        let signature = unfail failableSignature
        return signature
        where
            unfail :: CryptoFailable (Signature Curve_P521R1) -> Signature Curve_P521R1
            unfail (CryptoPassed signature) = signature
            unfail (CryptoFailed error_) = error $ show error_