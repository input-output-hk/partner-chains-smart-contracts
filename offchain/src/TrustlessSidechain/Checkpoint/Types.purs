module TrustlessSidechain.Checkpoint.Types
  ( CheckpointDatum(CheckpointDatum)
  , CheckpointParameter(CheckpointParameter)
  , InitCheckpointMint(InitCheckpointMint)
  , CheckpointRedeemer(CheckpointRedeemer)
  , CheckpointEndpointParam(CheckpointEndpointParam)
  , CheckpointMessage(CheckpointMessage)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(..)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (AssetClass)
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)

newtype CheckpointDatum = CheckpointDatum
  { blockHash ∷ ByteArray
  , blockNumber ∷ BigInt
  }

derive instance Generic CheckpointDatum _
derive instance Newtype CheckpointDatum _
instance ToData CheckpointDatum where
  toData (CheckpointDatum { blockHash, blockNumber }) = Constr
    zero
    [ toData blockHash, toData blockNumber ]

instance FromData CheckpointDatum where
  fromData (Constr n [ a, b ])
    | n == zero =
        CheckpointDatum <$>
          ( { blockHash: _, blockNumber: _ }
              <$> fromData a
              <*> fromData b
          )
  fromData _ = Nothing

newtype CheckpointParameter = CheckpointParameter
  { sidechainParams ∷ SidechainParams
  , checkpointToken ∷ AssetClass
  }

derive instance Generic CheckpointParameter _
derive instance Newtype CheckpointParameter _
instance ToData CheckpointParameter where
  toData
    ( CheckpointParameter
        { sidechainParams, checkpointToken }
    ) = Constr zero
    [ toData sidechainParams
    , toData checkpointToken
    ]

newtype InitCheckpointMint = InitCheckpointMint
  { icTxOutRef ∷ TransactionInput }

derive instance Generic InitCheckpointMint _
derive instance Newtype InitCheckpointMint _
instance ToData InitCheckpointMint where
  toData (InitCheckpointMint { icTxOutRef }) =
    toData icTxOutRef

data CheckpointRedeemer = CheckpointRedeemer
  { committeeSignatures ∷ Array SidechainSignature
  , committeePubKeys ∷ Array SidechainPublicKey
  , newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  }

derive instance Generic CheckpointRedeemer _

instance ToData CheckpointRedeemer where
  toData
    ( CheckpointRedeemer
        { committeeSignatures
        , committeePubKeys
        , newCheckpointBlockHash
        , newCheckpointBlockNumber
        }
    ) = Constr zero
    [ toData committeeSignatures
    , toData committeePubKeys
    , toData newCheckpointBlockHash
    , toData newCheckpointBlockNumber
    ]

-- | `CheckpointEndpointParam` is the offchain parameter for the checkpoint endpoint
newtype CheckpointEndpointParam = CheckpointEndpointParam
  { sidechainParams ∷ SidechainParams
  , committeeSignatures ∷ Array (SidechainPublicKey /\ Maybe SidechainSignature)
  , newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  , sidechainEpoch ∷ BigInt
  }

derive newtype instance Show CheckpointEndpointParam
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
    ) = Constr zero
    [ toData sidechainParams
    , toData checkpointBlockHash
    , toData checkpointBlockNumber
    , toData sidechainEpoch
    ]
