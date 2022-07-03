import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.ByteString as DBS
import Control.Concurrent.MVar
import Data.Time.Clock
import Data.Binary

import NetworkRules
import CryptoMagic
import Console
import MinerState
import TBlock
import Data.Time (getCurrentTimeZone)
import qualified Crypto.PubKey.DSA as ECDSA
import Crypto.ECC (Curve_P521R1(Curve_P521R1))
import Data.ByteString (toStrict)


main :: IO ()
main = hspec $ do
    describe "CryptoMagic" $ do
        it "should take private and public keys from pair" $ do
            (priv, pub) <- createEncodedKeys
            getKeyFromPair Public (priv, pub) `shouldBe` pub
            getKeyFromPair Private (priv, pub) `shouldBe` priv
        context "when one of the keys is invalid" $ do
            it "should sign with only valid private key" $ property $ do
                (priv, pub) <- createEncodedKeys
                signMsg (DBS.take 10 priv) pub `shouldThrow` anyException
            
            it "should verify signature with valid public key" $ property $ do
                (priv, pub) <- createEncodedKeys
                signature <- signMsg priv pub 
                let result = verifyStringMsg pub pub signature        
                result `shouldBe` True
               
            it "should return False on failed signature" $ property $ do
                (priv, pub) <- createEncodedKeys
                signature <- signMsg priv pub 
                let result = verifyStringMsg (DBS.take 10 pub) pub signature
                result `shouldBe` False
    describe "NetworkRules" $ do
        context "when there are not enough coins to make transaction" $ do
            it "should return false if not enough coins" $ property $ do
                (priv, pub) <- createEncodedKeys
                let sender = hashFunc pub
                let initMinerState' = (MinerState {
                    blocks = [genesisBlock],
                    pendingTransactions =  [],
                    network = [],
                    keyPair = fallbackPair,
                    hashId = fallbackHash,
                    shouldExit = False
                })
                --let recv = hashFunc priv
                --cur <- getCurrentTime
                --let candidate = TransactionCandidate sender recv 1 cur
                --signature <- signMsg priv (DBS.toStrict $ encode candidate)
                --let trans = Transaction sender recv 1 cur (pub, signature)
                checkEnoughCoins initMinerState' sender 10 `shouldBe` False
        context "when the blockchain is correct & all transactions are correct" $ do
            it "should not validate wrong blockchain" $ property $ do
                let initMinerState' = (MinerState {
                    blocks = [genesisBlock, genesisBlock, genesisBlock],
                    pendingTransactions =  [],
                    network = [],
                    keyPair = fallbackPair,
                    hashId = hashFunc $ fst fallbackPair,
                    shouldExit = False
                })
                validateWholeChain (blocks initMinerState') [] `shouldBe` False
            it "should validate right empty block" $ property $ do
                (priv, pub) <- createEncodedKeys
                let sender = hashFunc pub
                emptyBlock <- mineBlock (Block (hashFunc $ toStrict $ encode genesisBlock) sender 0 0 [])
                let initMinerState' = (MinerState {
                    blocks = [emptyBlock, genesisBlock],
                    pendingTransactions =  [],
                    network = [],
                    keyPair = (priv, pub),
                    hashId = hashFunc pub,
                    shouldExit = False
                })
                validateWholeChain (blocks initMinerState') [] `shouldBe` True
            it "should validate non-empty blocks" $ property $ do
                (priv, pub) <- createEncodedKeys
                (priv1, pub1) <- createEncodedKeys
                
                let sender = hashFunc pub
                let recv = hashFunc pub1
                cur <- getCurrentTime
                let candidate = TransactionCandidate sender recv 1 cur
                signature <- signMsg priv (DBS.toStrict $ encode candidate)
                let trans = Transaction sender recv 1 cur (pub, signature)
                --cur <- getCurrentTime
                --let trans1 = Transaction sender recv 1 cur (pub, signature)
                
                emptyBlock <- mineBlock (Block (hashFunc $ toStrict $ encode genesisBlock) sender 0 0 [])
                nonEmptyBlock <- mineBlock (Block (hashFunc $ toStrict $ encode emptyBlock) sender 0 1 [trans])
                
                let initMinerState' = (MinerState {
                    blocks = [nonEmptyBlock, emptyBlock, genesisBlock],
                    pendingTransactions =  [],
                    network = [],
                    keyPair = (priv, pub),
                    hashId = hashFunc pub,
                    shouldExit = False
                })
                validateWholeChain (blocks initMinerState') [] `shouldBe` True