module TrustlessSidechain.Checkpoint
  ( module ExportTypes
  , module ExportUtils
  , saveCheckpoint
  , getCheckpointPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.Map as Map
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointDatum(CheckpointDatum)
  , CheckpointEndpointParam(CheckpointEndpointParam)
  , CheckpointMessage(CheckpointMessage)
  , CheckpointParameter(CheckpointParameter)
  , CheckpointRedeemer(CheckpointRedeemer)
  , InitCheckpointMint(InitCheckpointMint)
  )
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointDatum(CheckpointDatum)
  , CheckpointEndpointParam(CheckpointEndpointParam)
  , CheckpointMessage(CheckpointMessage)
  , InitCheckpointMint(InitCheckpointMint)
  ) as ExportTypes
import TrustlessSidechain.Checkpoint.Utils
  ( checkpointPolicy
  , checkpointValidator
  , findCheckpointUtxo
  , initCheckpointMintTn
  , serialiseCheckpointMessage
  )
import TrustlessSidechain.Checkpoint.Utils
  ( checkpointPolicy
  , checkpointValidator
  , initCheckpointMintTn
  , serialiseCheckpointMessage
  ) as ExportUtils
import TrustlessSidechain.CommitteeATMSSchemes
  ( CommitteeATMSParams(CommitteeATMSParams)
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (assetClass, assetClassValue)
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging
  ( InternalError(InvalidScript, NotFoundUtxo, ConversionError)
  , OffchainError(InternalError)
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)

saveCheckpoint ∷ CheckpointEndpointParam → Contract TransactionHash
saveCheckpoint
  ( CheckpointEndpointParam
      { sidechainParams
      , aggregateSignature
      , newCheckpointBlockHash
      , newCheckpointBlockNumber
      , sidechainEpoch
      }
  ) = do
  -- Set up for the committee ATMS schemes
  ------------------------------------
  { committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy sidechainParams

  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator:
            (unwrap sidechainParams).thresholdNumerator
        , thresholdDenominator:
            (unwrap sidechainParams).thresholdDenominator
        , committeeOraclePolicy: committeeOracleCurrencySymbol
        }
  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicy
      committeeCertificateMint
      aggregateSignature

  -- Find the UTxO with the current committee.
  ------------------------------------
  { merkleRootTokenCurrencySymbol } ← MerkleRoot.getMerkleRootTokenMintingPolicy
    { sidechainParams, committeeCertificateVerificationCurrencySymbol }
  currentCommitteeUtxo ←
    liftedM
      ( show $ InternalError $ NotFoundUtxo
          "failed to find current committee UTxO"
      )
      $ UpdateCommitteeHash.findUpdateCommitteeHashUtxo
      $ UpdateCommitteeHash
          { sidechainParams
          , committeeOracleCurrencySymbol
          , committeeCertificateVerificationCurrencySymbol
          , merkleRootTokenCurrencySymbol
          }

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
    liftContractM
      ( show $ InternalError $ ConversionError
          "failed serializing the MerkleRootInsertionMessage"
      )
      $ serialiseCheckpointMessage checkpointMessage

  atmsLookupsAndConstraints ←
    CommitteeATMSSchemes.atmsSchemeLookupsAndConstraints
      $ CommitteeATMSParams
          { currentCommitteeUtxo
          , committeeCertificateMint
          , aggregateSignature
          , message: Utils.Crypto.sidechainMessageToTokenName scMsg
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

  balanceSignAndSubmit "Save Checkpoint" lookups constraints

saveCheckpointLookupsAndConstraints ∷
  { sidechainParams ∷ SidechainParams
  , newCheckpointBlockHash ∷ ByteArray
  , newCheckpointBlockNumber ∷ BigInt
  , sidechainEpoch ∷ BigInt
  , committeeCertificateVerificationCurrencySymbol ∷ CurrencySymbol
  } →
  Contract
    { lookupsAndConstraints ∷
        { constraints ∷ TxConstraints Void Void
        , lookups ∷ ScriptLookups Void
        }
    , checkpointMessage ∷ CheckpointMessage
    }
saveCheckpointLookupsAndConstraints
  { sidechainParams
  , newCheckpointBlockHash
  , newCheckpointBlockNumber
  , committeeCertificateVerificationCurrencySymbol
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
  { checkpointCurrencySymbol, checkpointTokenName } ← getCheckpointPolicy
    sidechainParams

  { committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy sidechainParams

  -- Getting checkpoint validator
  let
    checkpointParam = CheckpointParameter
      { sidechainParams
      , checkpointAssetClass: assetClass checkpointCurrencySymbol
          checkpointTokenName
      , committeeOracleCurrencySymbol
      , committeeCertificateVerificationCurrencySymbol
      }
  validator ← checkpointValidator checkpointParam
  let checkpointValidatorHash = Scripts.validatorHash validator

  -- Getting the checkpoint utxo
  checkpointUtxoLookup ← findCheckpointUtxo checkpointParam
  { index: checkpointOref
  , value: checkpointTxOut
  } ←
    liftContractM
      (show $ InternalError $ NotFoundUtxo "Failed to find checkpoint UTxO")
      checkpointUtxoLookup

  -- Building / submitting the transaction.
  let
    newCheckpointDatum = Datum $ toData
      ( CheckpointDatum
          { blockHash: newCheckpointBlockHash
          , blockNumber: newCheckpointBlockNumber
          }
      )
    value = assetClassValue (unwrap checkpointParam).checkpointAssetClass one
    redeemer = Redeemer $ toData
      ( CheckpointRedeemer
          { newCheckpointBlockHash
          , newCheckpointBlockNumber
          }
      )

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs (Map.singleton checkpointOref checkpointTxOut)
        <> Lookups.validator validator

    constraints = TxConstraints.mustSpendScriptOutput checkpointOref redeemer
      <> TxConstraints.mustPayToScript checkpointValidatorHash newCheckpointDatum
        DatumInline
        value

  pure
    { lookupsAndConstraints: { constraints, lookups }
    , checkpointMessage
    }

-- | `getCheckpointPolicy` grabs the checkpoint policy, currency symbol and token name
-- | (potentially throwing an error in the case that it is not possible).
getCheckpointPolicy ∷
  SidechainParams →
  Contract
    { checkpointPolicy ∷ MintingPolicy
    , checkpointCurrencySymbol ∷ CurrencySymbol
    , checkpointTokenName ∷ TokenName
    }
getCheckpointPolicy (SidechainParams sp) = do
  checkpointPolicy ← checkpointPolicy $
    InitCheckpointMint { icTxOutRef: sp.genesisUtxo }
  checkpointCurrencySymbol ← liftContractM
    ( show
        (InternalError (InvalidScript "Failed to get checkpoint CurrencySymbol"))
    )
    (Value.scriptCurrencySymbol checkpointPolicy)
  let checkpointTokenName = initCheckpointMintTn
  pure
    { checkpointPolicy, checkpointCurrencySymbol, checkpointTokenName }
