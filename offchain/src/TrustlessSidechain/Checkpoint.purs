module TrustlessSidechain.Checkpoint
  ( module ExportTypes
  , module ExportUtils
  , saveCheckpoint
  ) where

import Contract.Prelude hiding (unit)

import Contract.PlutusData (toData, RedeemerDatum(RedeemerDatum))
import Cardano.Types.PlutusData (unit)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Cardano.Types.BigNum as BigNum
import Contract.ScriptLookups as Lookups
import Cardano.Types.PlutusScript as PlutusScript
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Cardano.Types.ScriptHash (ScriptHash)
import JS.BigInt (BigInt)
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointDatum(CheckpointDatum)
  , CheckpointEndpointParam(CheckpointEndpointParam)
  , CheckpointMessage(CheckpointMessage)
  ) as ExportTypes
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointDatum(CheckpointDatum)
  , CheckpointEndpointParam(CheckpointEndpointParam)
  , CheckpointMessage(CheckpointMessage)
  , CheckpointParameter(CheckpointParameter)
  )
import TrustlessSidechain.Checkpoint.Utils
  ( burnOneCheckpointInitToken
  , checkpointAssetClass
  , checkpointCurrencyInfo
  , checkpointNftTn
  , checkpointValidator
  , mintOneCheckpointInitToken
  , serialiseCheckpointMessage
  ) as ExportUtils
import TrustlessSidechain.Checkpoint.Utils
  ( checkpointAssetClass
  , checkpointValidator
  , findCheckpointUtxo
  , serialiseCheckpointMessage
  )
import TrustlessSidechain.CommitteeATMSSchemes
  ( CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Util as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(NotFoundUtxo, ConversionError)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))
import Cardano.Types.Value as Value

saveCheckpoint ∷
  ∀ r.
  CheckpointEndpointParam →
  Run (APP + r) TransactionHash
saveCheckpoint
  ( CheckpointEndpointParam
      { sidechainParams
      , aggregateSignature
      , newCheckpointBlockHash
      , newCheckpointBlockNumber
      , sidechainEpoch
      }
  ) = do
  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator:
            (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator:
            (unwrap sidechainParams).thresholdDenominator
        }
  { currencySymbol: committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicy
      { committeeCertificateMint, sidechainParams }
      aggregateSignature

  currentCommitteeUtxo ←
    Effect.fromMaybeThrow
      ( NotFoundUtxo "failed to find current committee UTxO"
      )
      $ UpdateCommitteeHash.findUpdateCommitteeHashUtxo
          sidechainParams

  -- Grab the lookups + constraints for saving a merkle root
  ------------------------------------
  { lookupsAndConstraints
  , checkpointMessage
  } ← saveCheckpointLookupsAndConstraints
    { sidechainParams
    , newCheckpointBlockHash
    , newCheckpointBlockNumber
    , sidechainEpoch
    , committeeCertificateVerificationCurrencySymbol
    }

  -- Grab the lookups + constraints for the committee certificate
  -- verification
  ------------------------------------
  scMsg ←
    Run.note
      ( ConversionError "failed serializing the MerkleRootInsertionMessage"
      )
      $ serialiseCheckpointMessage checkpointMessage

  atmsLookupsAndConstraints ←
    CommitteeATMSSchemes.atmsSchemeLookupsAndConstraints sidechainParams
      $ CommitteeATMSParams
          { currentCommitteeUtxo
          , committeeCertificateMint
          , aggregateSignature
          , message: Utils.Crypto.ecdsaSecp256k1MessageToAssetName scMsg
          }

  -- Build / submit the transaction
  ------------------------------------
  let
    { lookups, constraints } = atmsLookupsAndConstraints
      <> lookupsAndConstraints
      <>
        -- include the current committee UTxO  in this transaction
        { lookups:
            Lookups.unspentOutputs
              ( Map.singleton currentCommitteeUtxo.index
                  currentCommitteeUtxo.value
              )
        , constraints:
            TxConstraints.mustReferenceOutput currentCommitteeUtxo.index
        }

  balanceSignAndSubmit "Save Checkpoint" { lookups, constraints }

saveCheckpointLookupsAndConstraints ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  , sidechainEpoch ∷ BigInt
  , committeeCertificateVerificationCurrencySymbol ∷ ScriptHash
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookupsAndConstraints ∷
        { constraints ∷ TxConstraints
        , lookups ∷ ScriptLookups
        }
    , checkpointMessage ∷ CheckpointMessage
    }
saveCheckpointLookupsAndConstraints
  { sidechainParams
  , newCheckpointBlockHash
  , newCheckpointBlockNumber
  , sidechainEpoch
  } = do
  -- Create the message to be signed
  -------------------------------------------------------------
  let
    checkpointMessage = CheckpointMessage
      { sidechainParams
      , checkpointBlockHash: newCheckpointBlockHash
      , checkpointBlockNumber: newCheckpointBlockNumber
      , sidechainEpoch
      }

  -- Getting the associated plutus scripts / UTXOs for checkpointing
  -------------------------------------------------------------
  checkpointAssetClass ← checkpointAssetClass
    sidechainParams

  -- Getting checkpoint validator
  let
    checkpointParam = CheckpointParameter
      { sidechainParams
      , checkpointAssetClass
      }
  versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams
  validator ← checkpointValidator checkpointParam versionOracleConfig
  let checkpointValidatorHash = PlutusScript.hash validator

  -- Getting the checkpoint utxo
  checkpointUtxoLookup ← findCheckpointUtxo checkpointParam
  { index: checkpointOref
  , value: checkpointTxOut
  } ←
    Run.note
      (NotFoundUtxo "Failed to find checkpoint UTxO")
      checkpointUtxoLookup

  -- Building / submitting the transaction.
  let
    newCheckpointDatum = toData
      ( CheckpointDatum
          { blockHash: newCheckpointBlockHash
          , blockNumber: newCheckpointBlockNumber
          }
      )
    value = Value.assetToValue (unwrap checkpointParam).checkpointAssetClass (BigNum.fromInt 1)

    lookups ∷ Lookups.ScriptLookups
    lookups =
      Lookups.unspentOutputs (Map.singleton checkpointOref checkpointTxOut)
        <> Lookups.validator validator

    constraints = TxConstraints.mustSpendScriptOutput checkpointOref (RedeemerDatum unit)
      <> TxConstraints.mustPayToScript checkpointValidatorHash newCheckpointDatum
        DatumInline
        value

  pure
    { lookupsAndConstraints: { constraints, lookups }
    , checkpointMessage
    }
