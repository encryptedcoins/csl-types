{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module CSL.Class where

import qualified Data.Map             as Map
import           Data.Maybe           (Maybe)
import           Data.Text            (unpack)
import           Ledger               (TxOutRef(..), TxId(..), DecoratedTxOut (..), toPubKeyHash, stakingCredential)
import           Ledger.Ada           (lovelaceValueOf)
import           Ledger.Value         (Value(..), TokenName (..), CurrencySymbol (..))
import           PlutusTx.AssocMap    hiding (mapMaybe)
import           PlutusTx.Prelude     hiding (Maybe)
import           Text.Hex             (decodeHex)
import           Text.Read            (readMaybe)

import qualified CSL
import           Utils.Address        (bech32ToAddress)

class FromCSL a b where
    fromCSL :: a -> Maybe b

instance FromCSL CSL.TransactionInput TxOutRef where
    fromCSL (CSL.TransactionInput txId ind) = do
        bs <- decodeHex txId
        return $ TxOutRef (TxId $ toBuiltin bs) ind

instance FromCSL CSL.TransactionInputs [TxOutRef] where
    fromCSL = Just . mapMaybe fromCSL

instance FromCSL CSL.Value Value where
    fromCSL (CSL.Value lovelace mma) = do
        valADA <- fmap lovelaceValueOf $ readMaybe $ unpack lovelace
        let valMA = case mma of
                Nothing -> zero
                Just (CSL.MultiAsset ma) -> Value (g' ma)
        return $ valADA + valMA
        where
            f  (aTxt, bTxt) = do
                a <- TokenName . toBuiltin <$> decodeHex aTxt
                b <- readMaybe $ unpack bTxt :: Maybe Integer
                return (a, b)
            g  = fromList . mapMaybe f . Map.toList
            f' (kTxt, m) = do
                k <- CurrencySymbol . toBuiltin <$> decodeHex kTxt
                return (k, g m)
            g' = fromList . mapMaybe f' . Map.toList

instance FromCSL CSL.TransactionOutput DecoratedTxOut where
    fromCSL (CSL.TransactionOutput addrCSL valCSL _ _) = do
        addr <- bech32ToAddress addrCSL
        val  <- fromCSL valCSL
        let scr = stakingCredential addr
        case toPubKeyHash addr of
          Nothing  -> Nothing
          Just pkh -> return $ PublicKeyDecoratedTxOut pkh scr val Nothing Nothing

instance FromCSL CSL.TransactionOutputs [DecoratedTxOut] where
    fromCSL = Just . mapMaybe fromCSL

instance FromCSL CSL.TransactionUnspentOutput (TxOutRef, DecoratedTxOut) where
    fromCSL (CSL.TransactionUnspentOutput input output) = do
        ref   <- fromCSL input
        txOut <- fromCSL output
        return (ref, txOut)
    
instance FromCSL CSL.TransactionUnspentOutputs [(TxOutRef, DecoratedTxOut)] where
    fromCSL = Just . mapMaybe fromCSL