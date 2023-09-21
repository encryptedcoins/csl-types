{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import           CSL.Class                     (FromCSL (fromCSL), ToCSL (toCSL))
import           Cardano.Api                   (NetworkId (Mainnet, Testnet), NetworkMagic (NetworkMagic), valueFromList,
                                                valueToList)
import qualified Cardano.Api                   as C
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Maybe                    (isJust)
import           Ledger                        (Address (Address), DecoratedTxOut (..), PubKeyHash, StakingCredential, TxOutRef,
                                                Value)
import           Ledger.Value.CardanoAPI       (fromCardanoValue, toCardanoValue)
import           Plutus.PAB.Arbitrary          ()
import           Plutus.V1.Ledger.Api          (Credential (..))
import           Plutus.V2.Ledger.Api          (getValue)
import qualified Plutus.V2.Ledger.Api          as P
import           PlutusAppsExtra.Utils.Address (addressToBech32)
import qualified PlutusTx.AssocMap             as PAM
import           Test.Hspec                    (Expectation, Spec, describe, hspec, it, shouldBe)
import           Test.QuickCheck               (Arbitrary (arbitrary), Testable (property), generate, listOf, oneof)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "toCSL . fromCSL == id" $ do
    it "TxOutRef" $ property propTxOutRef
    it "[TxOutRef]" $ property propTxOutRefs
    it "Value" $ property propValue
    it "DecoratedTxOut" $ property propDecoratedTxOut
    it "[DecoratedTxOut]" $ property propDecoratedTxOuts
    it "(TxOutRef, DecoratedTxOut)" $ property propRefWithDecoratedTxOut
    it "[TxOutRef, DecoratedTxOut]" $ property propRefsWithDecoratedTxOuts

propTxOutRef :: TxOutRef -> Expectation
propTxOutRef ref = (toCSL ref >>= fromCSL) `shouldBe` Just ref

propTxOutRefs :: [TxOutRef] -> Expectation
propTxOutRefs refs = (toCSL refs >>= fromCSL) `shouldBe` Just refs

propValue :: Value' -> Expectation
propValue (Value' val) = (toCSL (fromCardanoValue val) >>= fromCSL) `shouldBe` Just (fromCardanoValue val)

propDecoratedTxOut :: () -> Expectation
propDecoratedTxOut _ = do
    (PublicKeyDecoratedTxOut' txOut, NetworkId' network) <- liftIO $ generate arbitrary
    (toCSL (txOut, network) >>= fromCSL) `shouldBe` Just txOut

propDecoratedTxOuts :: () -> Expectation
propDecoratedTxOuts _ = do
    (txOuts, NetworkId' network) <- fmap (first (fmap unPKDO')) $ liftIO $ generate $ arbitrary @([PublicKeyDecoratedTxOut'], NetworkId')
    (toCSL (txOuts, network) >>= fromCSL) `shouldBe` Just txOuts

propRefWithDecoratedTxOut :: TxOutRef -> Expectation
propRefWithDecoratedTxOut ref = do
    (PublicKeyDecoratedTxOut' txOut, NetworkId' network) <- liftIO $ generate arbitrary
    (toCSL (ref, txOut, network) >>= fromCSL) `shouldBe` Just (ref, txOut)

propRefsWithDecoratedTxOuts :: [TxOutRef] -> Expectation
propRefsWithDecoratedTxOuts refs = do
    (txOuts, NetworkId' network) <- fmap (first (fmap unPKDO')) $ liftIO $ generate arbitrary
    let xs = zip refs txOuts
    (toCSL (xs, network) >>= fromCSL) `shouldBe` Just xs

newtype Value' = Value' C.Value
    deriving newtype Show

instance Arbitrary Value' where
    arbitrary = do
        val <- arbitrary
        if validValue val then pure $ Value' val else arbitrary
        where validValue val = fromCardanoValue val == (P.Value . PAM.fromList . PAM.toList $ getValue $ fromCardanoValue val)

newtype PublicKeyDecoratedTxOut' = PublicKeyDecoratedTxOut' {unPKDO' :: DecoratedTxOut}
    deriving newtype Show

instance {-# OVERLAPPING #-} Arbitrary (PublicKeyDecoratedTxOut', NetworkId') where
    arbitrary = do
        (NetworkId' network) <- arbitrary
        pkh <- arbitrary
        sc  <- arbitrary
        if not $ validAddr network pkh sc then arbitrary else do
            Value' val <- arbitrary
            pure $ (,NetworkId' network) $ PublicKeyDecoratedTxOut' $ PublicKeyDecoratedTxOut pkh sc val Nothing Nothing
        where validAddr network pkh sc = isJust $ addressToBech32 network $ Address (PubKeyCredential pkh) sc

instance {-# OVERLAPPING #-} Arbitrary ([PublicKeyDecoratedTxOut'], NetworkId') where
    arbitrary = do
        res <- unzip <$> listOf arbitrary
        pure $ head <$> res

newtype NetworkId' = NetworkId' NetworkId
    deriving newtype Show

instance Arbitrary NetworkId' where
    arbitrary = fmap NetworkId' $ oneof $ map pure [Mainnet, Testnet $ NetworkMagic 1, Testnet $ NetworkMagic 2]