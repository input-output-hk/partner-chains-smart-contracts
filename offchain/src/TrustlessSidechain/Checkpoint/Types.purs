module TrustlessSidechain.Checkpoint.Types
  ( CheckpointDatum(CheckpointDatum)
  , CheckpointParameter(CheckpointParameter)
  , InitCheckpointMint(InitCheckpointMint)
  , CheckpointRedeemer(CheckpointRedeemer)
  , CheckpointEndpointParam(CheckpointEndpointParam)
  , CheckpointMessage(CheckpointMessage)
  ) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import TrustlessSidechain.CommitteeATMSSchemes.Types (ATMSAggregateSignatures)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (AssetClass)
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productFromData4
  , productToData2
  , productToData4
  )

newtype CheckpointDatum = CheckpointDatum
  { blockHash ∷ ByteArray
  , blockNumber ∷ BigInt
  }

derive instance Generic CheckpointDatum _
derive instance Newtype CheckpointDatum _
derive newtype instance Eq CheckpointDatum
derive newtype instance Show CheckpointDatum

instance ToData CheckpointDatum where
  toData (CheckpointDatum { blockHash, blockNumber }) =
    productToData2 blockHash blockNumber

instance FromData CheckpointDatum where
  fromData = productFromData2
    ( \x y →
        CheckpointDatum { blockHash: x, blockNumber: y }
    )

newtype CheckpointParameter = CheckpointParameter
  { sidechainParams ∷ SidechainParams
  , checkpointAssetClass ∷ AssetClass
  , committeeOracleCurrencySymbol ∷ CurrencySymbol
  , committeeCertificateVerificationCurrencySymbol ∷ CurrencySymbol
  }

derive instance Generic CheckpointParameter _

derive instance Newtype CheckpointParameter _

derive newtype instance Eq CheckpointParameter

instance ToData CheckpointParameter where
  toData
    ( CheckpointParameter
        { sidechainParams
        , checkpointAssetClass
        , committeeOracleCurrencySymbol
        , committeeCertificateVerificationCurrencySymbol
        }
    ) = productToData4 sidechainParams
    checkpointAssetClass
    committeeOracleCurrencySymbol
    committeeCertificateVerificationCurrencySymbol

instance FromData CheckpointParameter where
  fromData = productFromData4 $
    \sidechainParams
     checkpointAssetClass
     committeeOracleCurrencySymbol
     committeeCertificateVerificationCurrencySymbol → CheckpointParameter
      { sidechainParams
      , checkpointAssetClass
      , committeeOracleCurrencySymbol
      , committeeCertificateVerificationCurrencySymbol
      }

derive newtype instance Show CheckpointParameter

newtype InitCheckpointMint = InitCheckpointMint
  { icTxOutRef ∷ TransactionInput }

derive instance Generic InitCheckpointMint _

derive instance Newtype InitCheckpointMint _

derive newtype instance Eq InitCheckpointMint

derive newtype instance Show InitCheckpointMint

instance ToData InitCheckpointMint where
  toData (InitCheckpointMint { icTxOutRef }) =
    toData icTxOutRef

instance FromData InitCheckpointMint where
  fromData = map (InitCheckpointMint <<< { icTxOutRef: _ }) <$> fromData

newtype CheckpointRedeemer = CheckpointRedeemer
  { newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  }

derive instance Generic CheckpointRedeemer _

derive newtype instance Eq CheckpointRedeemer

derive newtype instance Show CheckpointRedeemer

instance ToData CheckpointRedeemer where
  toData
    ( CheckpointRedeemer
        { newCheckpointBlockHash
        , newCheckpointBlockNumber
        }
    ) = productToData2
    newCheckpointBlockHash
    newCheckpointBlockNumber

instance FromData CheckpointRedeemer where
  fromData = productFromData2
    $ \newCheckpointBlockHash newCheckpointBlockNumber →
        CheckpointRedeemer { newCheckpointBlockHash, newCheckpointBlockNumber }

-- | `CheckpointEndpointParam` is the offchain parameter for the checkpoint endpoint
newtype CheckpointEndpointParam = CheckpointEndpointParam
  { sidechainParams ∷ SidechainParams
  , aggregateSignature ∷ ATMSAggregateSignatures
  , newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  , sidechainEpoch ∷ BigInt
  }

derive instance Newtype CheckpointEndpointParam _

newtype CheckpointMessage = CheckpointMessage
  { sidechainParams ∷ SidechainParams
  , checkpointBlockHash ∷ ByteArray
  , checkpointBlockNumber ∷ BigInt
  , sidechainEpoch ∷ BigInt
  }

derive instance Generic CheckpointMessage _

derive newtype instance Eq CheckpointMessage

derive newtype instance Show CheckpointMessage

instance ToData CheckpointMessage where
  toData
    ( CheckpointMessage
        { sidechainParams
        , checkpointBlockHash
        , checkpointBlockNumber
        , sidechainEpoch
        }
    ) = Constr (BigNum.fromInt 0)
    [ toData sidechainParams
    , toData checkpointBlockHash
    , toData checkpointBlockNumber
    , toData sidechainEpoch
    ]

instance FromData CheckpointMessage where
  fromData = case _ of
    Constr tag [ t1, t2, t3, t4 ] | tag == BigNum.fromInt 0 → do
      sidechainParams ← fromData t1
      checkpointBlockHash ← fromData t2
      checkpointBlockNumber ← fromData t3
      sidechainEpoch ← fromData t4
      pure $ CheckpointMessage
        { sidechainParams
        , checkpointBlockHash
        , checkpointBlockNumber
        , sidechainEpoch
        }
    _ → Nothing
