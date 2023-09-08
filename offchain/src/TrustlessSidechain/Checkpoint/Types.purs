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

instance ToData InitCheckpointMint where
  toData (InitCheckpointMint { icTxOutRef }) =
    toData icTxOutRef

data CheckpointRedeemer = CheckpointRedeemer
  { newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  }

derive instance Generic CheckpointRedeemer _

instance ToData CheckpointRedeemer where
  toData
    ( CheckpointRedeemer
        { newCheckpointBlockHash
        , newCheckpointBlockNumber
        }
    ) = productToData2
    newCheckpointBlockHash
    newCheckpointBlockNumber

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
