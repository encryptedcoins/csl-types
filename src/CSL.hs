{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module CSL where

import           Data.Aeson          (ToJSON, FromJSON)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           PlutusTx.Prelude    (BuiltinByteString)
import           Prelude

data TransactionInput = TransactionInput {
        transaction_id :: BuiltinByteString,
        index          :: Integer
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

type TransactionInputs = [TransactionInput]

type MultiAsset = Map.Map Text (Map.Map Text Text)

data Value = Value
    {
        coin        :: Text,
        multiasset  :: Maybe MultiAsset
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

data TransactionOutput = TransactionOutput
    {
        address     :: Text,
        amount      :: Value,
        plutus_data :: Maybe BuiltinByteString,
        script_ref  :: Maybe BuiltinByteString
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

type TransactionOutputs = [TransactionOutput]

data TransactionUnspentOutput = TransactionUnspentOutput
    {
        input :: TransactionInput,
        output :: TransactionOutput
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON)

type TransactionUnspentOutputs = [TransactionUnspentOutput]